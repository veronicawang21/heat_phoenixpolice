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

this <- "call_cold"
this_lab <- "Police emergency calls"

###################################################################################
###################################################################################
## overall : total calls
###################################################################################
###################################################################################

t <- outcome %>%
  group_by(date_only) %>%
  summarise(n=n())

names(t) <- c("date", "n")

dat <- t %>%
  left_join(prism) %>%
  mutate(date_num=as.numeric(date)) %>%
  mutate(mo=month(date)) %>%
  filter(!(mo %in% 5:10)) %>%
  mutate(wd=weekdays(date)) %>%
  select(date, n, tmax, tmax_lag1:tmax_lag3, tmin, tmin_lag1:tmin_lag3, 
         himax, himax_lag1:himax_lag3, himin, himin_lag1:himin_lag3,
         date_num, mo, wd)

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
  labs(title = "Police emergency calls: cold months", x="Celsius", y="Relative risk") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi, fill=t_hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) +
  facet_nested(.~m+lag) + theme_bw() + 
  ylim(0.8, 1.4) +
  theme(legend.title= element_blank(),
        legend.position="bottom")

# save subplot into new folder named 'parts' to be combined later for supplemental figures
saveRDS(p_type, paste("...parts\\", this, "_rr.rds",sep = ""))

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
  labs(title = "Police emergency calls: cold months", x="Celsius", y="Count Difference") +
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
