rm(list = ls())


setwd("")

library(tidyverse)
library(stringr)
library(ggeffects)
library(ggplot2)
library(weathermetrics) 
library(splines)
library(mgcv)
library(dlnm)
library(ggh4x)

###################################################################################
###################################################################################
### ANALYSES
###################################################################################
###################################################################################

prism <- readRDS("prism.rds")
outcome <- readRDS("incident_key.rds")

reason_group <- c("violent", "nonviolent", "vehicle", "domestic", "health", "sex", "other")
lab_group <- c("Violent crimes", "Non-violent crimes", "Road incidents", "Domestic incidents",
               "Health incidents", "Sexual incidents", "Other incidents")

# select emergency type
this <- reason_group[1]
this_lab <- lab_group[which(reason_group==this)]

###################################################################################
###################################################################################
## overall : total calls
###################################################################################
###################################################################################

t <- outcome %>%
  mutate(type_agg = recode(group, "health_mental"="health",
                             "health_non"="health")) %>%
  group_by(date_only, type_agg) %>%
  summarise(n=n())

names(t) <- c("date", "type", "n")

dat <- t %>%
  left_join(prism) %>%
  mutate(date_num=as.numeric(date)) %>%
  mutate(mo=month(date)) %>%
  mutate(wd=weekdays(date)) %>%
  select(date, n, tmax, tmax_lag1:tmax_lag3, tmin, tmin_lag1:tmin_lag3, 
         himax, himax_lag1:himax_lag3, himin, himin_lag1:himin_lag3,
         date_num, mo, wd, type) %>%
  ungroup()

## specify reason
dat <- dat %>% filter(type==this) 

tmax.hist.obs <- dat %>% select(tmax, tmax_lag1:tmax_lag3)
tmin.hist.obs <- dat %>% select(tmin, tmin_lag1:tmin_lag3)
himax.hist.obs <- dat %>% select(himax, himax_lag1:himax_lag3)
himin.hist.obs <- dat %>% select(himin, himin_lag1:himin_lag3)
cb.tmax.obs <- crossbasis(tmax.hist.obs, argvar = list(df=3), arglag = list(df=3))
cb.tmin.obs <- crossbasis(tmin.hist.obs, argvar = list(df=3), arglag = list(df=3))
cb.himax.obs <- crossbasis(himax.hist.obs, argvar = list(df=3), arglag = list(df=3))
cb.himin.obs <- crossbasis(himin.hist.obs, argvar = list(df=3), arglag = list(df=3))

###################################################################################
###################################################################################
## RR
###################################################################################
###################################################################################

make_long <- function(wide, nam){
  long <- wide %>% 
    pivot_longer(
      cols = "lag0":"lag3", 
      names_to = "lag",
      values_to = nam
    )
  return(long)
}

process_rr <- function(cross_pred){
  df_est <- data.frame(deg=row.names(cross_pred$matRRfit),cross_pred$matRRfit)
  df_est <- make_long(df_est, "est")
  df_lo <- data.frame(deg=row.names(cross_pred$matRRlow),cross_pred$matRRlow)
  df_lo <- make_long(df_lo,"lo")
  df_hi <- data.frame(deg=row.names(cross_pred$matRRhigh),cross_pred$matRRhigh)
  df_hi <- make_long(df_hi,"hi")
  
  df_res <- df_est %>%
    left_join(df_lo) %>%
    left_join(df_hi) %>%
    mutate(deg=as.numeric(deg))
  return(df_res)
}

process_cd <- function(cross_pred){
  df_est <- data.frame(deg=row.names(cross_pred$matfit),cross_pred$matfit)
  df_est <- make_long(df_est, "est")
  df_lo <- data.frame(deg=row.names(cross_pred$matlow),cross_pred$matlow)
  df_lo <- make_long(df_lo,"lo")
  df_hi <- data.frame(deg=row.names(cross_pred$mathigh),cross_pred$mathigh)
  df_hi <- make_long(df_hi,"hi")
  
  df_res <- df_est %>%
    left_join(df_lo) %>%
    left_join(df_hi) %>%
    mutate(deg=as.numeric(deg))
  return(df_res)
}

## process res: RR
###################################################################################
mod_tmax <- glm(n ~ cb.tmax.obs + ns(date_num, df=48) + as.factor(wd), family=quasipoisson, data=dat)
mod_tmin <- glm(n ~ cb.tmin.obs + ns(date_num, df=48) + as.factor(wd), family=quasipoisson, data=dat)
mod_himax <- glm(n ~ cb.himax.obs + ns(date_num, df=48) + as.factor(wd), family=quasipoisson, data=dat)
mod_himin <- glm(n ~ cb.himin.obs + ns(date_num, df=48) + as.factor(wd), family=quasipoisson, data=dat)

pred_tmax <- crosspred(cb.tmax.obs, mod_tmax, cen=8, by=1)
pred_tmin <- crosspred(cb.tmin.obs, mod_tmin, cen=0, by=1)
pred_himax <- crosspred(cb.himax.obs, mod_himax, cen=8, by=1)
pred_himin <- crosspred(cb.himin.obs, mod_himin, cen=0, by=1)

## get cumulative associations (10th to 50th percentile)
# pred_tmax <- crosspred(cb.tmax.obs, mod_tmax, cen=18.4, by=0.2)
# paste(round(pred_tmax$allRRfit[which(names(pred_tmax$allRRfit) == "30.6")],2), " (",
# round(pred_tmax$allRRlow[which(names(pred_tmax$allRRlow) == "30.6")],2), ", ",
# round(pred_tmax$allRRhigh[which(names(pred_tmax$allRRhigh) == "30.6")],2), ")", sep="")
# pred_tmax <- crosspred(cb.tmax.obs, mod_tmax, cen=30.6, by=0.1)
# paste(round(pred_tmax$allRRfit[which(names(pred_tmax$allRRfit) == "41.6")],2), " (",
# round(pred_tmax$allRRlow[which(names(pred_tmax$allRRlow) == "41.6")],2), ", ",
# round(pred_tmax$allRRhigh[which(names(pred_tmax$allRRhigh) == "41.6")],2), ")", sep="")
# pred_tmin <- crosspred(cb.tmin.obs, mod_tmin, cen=5.9, by=0.1)
# paste(round(pred_tmin$allRRfit[which(names(pred_tmin$allRRfit) == "16.3")],2), " (",
# round(pred_tmin$allRRlow[which(names(pred_tmin$allRRlow) == "16.3")],2), ", ",
# round(pred_tmin$allRRhigh[which(names(pred_tmin$allRRhigh) == "16.3")],2), ")", sep="")
# pred_tmin <- crosspred(cb.tmin.obs, mod_tmin, cen=16.3, by=0.1)
# paste(round(pred_tmin$allRRfit[which(names(pred_tmin$allRRfit) == "28.7")],2), " (",
# round(pred_tmin$allRRlow[which(names(pred_tmin$allRRlow) == "28.7")],2), ", ",
# round(pred_tmin$allRRhigh[which(names(pred_tmin$allRRhigh) == "28.7")],2), ")", sep="")
# 
# 
## get cumulative associations (50th to 90th percentile)
# pred_himax <- crosspred(cb.himax.obs, mod_himax, cen=18.4, by=0.2)
# paste(round(pred_himax$allRRfit[which(names(pred_himax$allRRfit) == "30.6")],2), " (",
# round(pred_himax$allRRlow[which(names(pred_himax$allRRlow) == "30.6")],2), ", ",
# round(pred_himax$allRRhigh[which(names(pred_himax$allRRhigh) == "30.6")],2), ")", sep="")
# pred_himax <- crosspred(cb.himax.obs, mod_himax, cen=30.6, by=0.1)
# paste(round(pred_himax$allRRfit[which(names(pred_himax$allRRfit) == "41.6")],2), " (",
# round(pred_himax$allRRlow[which(names(pred_himax$allRRlow) == "41.6")],2), ", ",
# round(pred_himax$allRRhigh[which(names(pred_himax$allRRhigh) == "41.6")],2), ")", sep="")
# pred_himin <- crosspred(cb.himin.obs, mod_himin, cen=5.9, by=0.1)
# paste(round(pred_himin$allRRfit[which(names(pred_himin$allRRfit) == "16.3")],2), " (",
# round(pred_himin$allRRlow[which(names(pred_himin$allRRlow) == "16.3")],2), ", ",
# round(pred_himin$allRRhigh[which(names(pred_himin$allRRhigh) == "16.3")],2), ")", sep="")
# pred_himin <- crosspred(cb.himin.obs, mod_himin, cen=16.3, by=0.1)
# paste(round(pred_himin$allRRfit[which(names(pred_himin$allRRfit) == "28.7")],2), " (",
# round(pred_himin$allRRlow[which(names(pred_himin$allRRlow) == "28.7")],2), ", ",
# round(pred_himin$allRRhigh[which(names(pred_himin$allRRhigh) == "28.7")],2), ")", sep="")

df_tmax <- process_rr(pred_tmax)
df_tmin <- process_rr(pred_tmin)
df_himax <- process_rr(pred_himax)
df_himin <- process_rr(pred_himin)

res <- rbind(cbind(df_tmax, expo="Maximum Temperature (ref=8C)", ref=8),
             cbind(df_tmin, expo="Minimum Temperature (ref=0C)", ref=0),
             cbind(df_himax, expo="Maximum Heat Index (ref=8C)", ref=8),
             cbind(df_himin, expo="Minimum Heat Index (ref=0C)", ref=0))

p_type <- res %>%
  mutate(m=ifelse(ref==0, "Minimum", "Maximum"),
         t_hi=ifelse(expo %in% c("Maximum Temperature (ref=8C)", "Minimum Temperature (ref=0C)"), "Temperature", "Heat index")) %>%
  ggplot(aes(group=t_hi, color=t_hi)) +
  geom_hline(yintercept=1, linetype="dashed", color = "grey", linewidth=0.8) +
  labs(title = this_lab, x="Celcius", y="Relative Risk") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi, fill=t_hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) +
  facet_nested(.~m+lag) + theme_bw() + 
  ylim(0.8, 1.4) +
  theme(legend.title= element_blank(),
        legend.position="bottom")

# save subplot into new folder named 'parts' to be combined later for supplemental figures
saveRDS(p_type, paste("...phoenix_policecalls\\parts\\", this, "_rr.rds",sep = ""))

# specific lags
# save subplot into new folder named 'main' to be combined later for main figure 1
res_lag <- res %>% filter(lag %in% c("lag0", "lag1")) %>% mutate(type=this)

saveRDS(res_lag, paste("...main\\", this, "_rr.rds",sep = ""))


## process res: Count Diff
###################################################################################
mod_tmax <- glm(n ~ cb.tmax.obs + ns(date_num, df=48) + as.factor(wd), family=quasipoisson(link = "identity"), data=dat)
mod_tmin <- glm(n ~ cb.tmin.obs + ns(date_num, df=48) + as.factor(wd), family=quasipoisson(link = "identity"), data=dat)
mod_himax <- glm(n ~ cb.himax.obs + ns(date_num, df=48) + as.factor(wd), family=quasipoisson(link = "identity"), data=dat)
mod_himin <- glm(n ~ cb.himin.obs + ns(date_num, df=48) + as.factor(wd), family=quasipoisson(link = "identity"), data=dat)

pred_tmax <- crosspred(cb.tmax.obs, mod_tmax, cen=8, by=1)
pred_tmin <- crosspred(cb.tmin.obs, mod_tmin, cen=0, by=1)
pred_himax <- crosspred(cb.himax.obs, mod_himax, cen=8, by=1)
pred_himin <- crosspred(cb.himin.obs, mod_himin, cen=0, by=1)

## get cumulative associations (10th to 50th percentile)
# pred_tmax <- crosspred(cb.tmax.obs, mod_tmax, cen=18.4, by=0.2)
# paste(round(pred_tmax$allfit[which(names(pred_tmax$allfit) == "30.6")],0), " (",
# round(pred_tmax$alllow[which(names(pred_tmax$alllow) == "30.6")],0), ", ",
# round(pred_tmax$allhigh[which(names(pred_tmax$allhigh) == "30.6")],0), ")", sep="")
# pred_tmax <- crosspred(cb.tmax.obs, mod_tmax, cen=30.6, by=0.1)
# paste(round(pred_tmax$allfit[which(names(pred_tmax$allfit) == "41.6")],0), " (",
# round(pred_tmax$alllow[which(names(pred_tmax$alllow) == "41.6")],0), ", ",
# round(pred_tmax$allhigh[which(names(pred_tmax$allhigh) == "41.6")],0), ")", sep="")
# pred_tmin <- crosspred(cb.tmin.obs, mod_tmin, cen=5.9, by=0.1)
# paste(round(pred_tmin$allfit[which(names(pred_tmin$allfit) == "16.3")],0), " (",
# round(pred_tmin$alllow[which(names(pred_tmin$alllow) == "16.3")],0), ", ",
# round(pred_tmin$allhigh[which(names(pred_tmin$allhigh) == "16.3")],0), ")", sep="")
# pred_tmin <- crosspred(cb.tmin.obs, mod_tmin, cen=16.3, by=0.1)
# paste(round(pred_tmin$allfit[which(names(pred_tmin$allfit) == "28.7")],0), " (",
# round(pred_tmin$alllow[which(names(pred_tmin$alllow) == "28.7")],0), ", ",
# round(pred_tmin$allhigh[which(names(pred_tmin$allhigh) == "28.7")],0), ")", sep="")
# 
# 
## get cumulative associations (50th to 90th percentile)
# pred_himax <- crosspred(cb.himax.obs, mod_himax, cen=18.4, by=0.2)
# paste(round(pred_himax$allfit[which(names(pred_himax$allfit) == "30.6")],0), " (",
# round(pred_himax$alllow[which(names(pred_himax$alllow) == "30.6")],0), ", ",
# round(pred_himax$allhigh[which(names(pred_himax$allhigh) == "30.6")],0), ")", sep="")
# pred_himax <- crosspred(cb.himax.obs, mod_himax, cen=30.6, by=0.1)
# paste(round(pred_himax$allfit[which(names(pred_himax$allfit) == "41.6")],0), " (",
# round(pred_himax$alllow[which(names(pred_himax$alllow) == "41.6")],0), ", ",
# round(pred_himax$allhigh[which(names(pred_himax$allhigh) == "41.6")],0), ")", sep="")
# pred_himin <- crosspred(cb.himin.obs, mod_himin, cen=5.9, by=0.1)
# paste(round(pred_himin$allfit[which(names(pred_himin$allfit) == "16.3")],0), " (",
# round(pred_himin$alllow[which(names(pred_himin$alllow) == "16.3")],0), ", ",
# round(pred_himin$allhigh[which(names(pred_himin$allhigh) == "16.3")],0), ")", sep="")
# pred_himin <- crosspred(cb.himin.obs, mod_himin, cen=16.3, by=0.1)
# paste(round(pred_himin$allfit[which(names(pred_himin$allfit) == "28.7")],0), " (",
# round(pred_himin$alllow[which(names(pred_himin$alllow) == "28.7")],0), ", ",
# round(pred_himin$allhigh[which(names(pred_himin$allhigh) == "28.7")],0), ")", sep="")

df_tmax <- process_cd(pred_tmax)
df_tmin <- process_cd(pred_tmin)
df_himax <- process_cd(pred_himax)
df_himin <- process_cd(pred_himin)

res <- rbind(cbind(df_tmax, expo="Maximum Temperature (ref=8C)", ref=8),
             cbind(df_tmin, expo="Minimum Temperature (ref=0C)", ref=0),
             cbind(df_himax, expo="Maximum Heat Index (ref=8C)", ref=8),
             cbind(df_himin, expo="Minimum Heat Index (ref=0C)", ref=0))

p_type <- res %>%
  mutate(m=ifelse(ref==0, "Minimum", "Maximum"),
         t_hi=ifelse(expo %in% c("Maximum Temperature (ref=8C)", "Minimum Temperature (ref=0C)"), "Temperature", "Heat index")) %>%
  ggplot(aes(group=t_hi, color=t_hi)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", linewidth=0.8) +
  # geom_vline(aes(xintercept=ref), linetype="dotted", color = "black", linewidth=0.8) +
  labs(title = this_lab, x="Celcius", y="Count Difference") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi, fill=t_hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  scale_y_continuous(
    "Count difference", 
    sec.axis = sec_axis(~ 100*. /mean(dat$n), name = "% of mean count")
  ) +
  scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) +
  facet_nested(.~m+lag) + theme_bw() + 
  theme(legend.title= element_blank(),
        legend.position="bottom")

# save subplot into new folder named 'parts' to be combined later for supplemental figures
saveRDS(p_type, paste("...parts\\", this, "_cd.rds",sep = ""))

# specific lags
# save subplot into new folder named 'main' to be combined later for main figure 2
res_lag <- res %>% mutate(avg=mean(dat$n)) %>% filter(lag %in% c("lag0", "lag1")) %>% mutate(type=this)

saveRDS(res_lag, paste("...main\\", this, "_cd.rds",sep = ""))
