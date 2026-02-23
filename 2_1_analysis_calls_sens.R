#####################################
#####################################
#### Sensitivity/other analyses #####
#####################################
#####################################

# clear environment
rm(list = ls())

# set working directory
setwd("")

# load in libraries needed
library(tidyverse)
library(stringr)
library(ggeffects)
library(ggplot2)
library(weathermetrics) 
library(splines)
library(mgcv)
library(dlnm)
library(ggh4x)
library(data.table)

# load in derived datasets (from 1_0_prep rds file)
## create more lags
prism <- readRDS("prism.rds")
setDT(prism)
prism <- prism[, paste0("tmax_lag", 7:13) := shift(tmax, 7:13), ]
prism <- prism[, paste0("tmin_lag", 7:13) := shift(tmin, 7:13), ]
prism <- prism[, paste0("himax_lag", 7:13) := shift(himax, 7:13), ]
prism <- prism[, paste0("himin_lag", 7:13) := shift(himin, 7:13), ]
prism <- as.data.frame(prism)
outcome <- readRDS("incident_key.rds")

###################################################################################
###################################################################################
### SENSITIVITY ANALYSES
###################################################################################
###################################################################################

this <- "call"
this_lab <- "Police emergency calls"

###################################################################################
###################################################################################
## 7-day lag (Figure S1)
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
  mutate(wd=weekdays(date)) %>%
  select(date, n, tmax, tmax_lag1:tmax_lag6, tmax_lag7:tmax_lag13, 
         tmin, tmin_lag1:tmin_lag6, tmin_lag7:tmin_lag13, 
         himax, himax_lag1:himax_lag6, himax_lag7:himax_lag13,
         himin, himin_lag1:himin_lag6, himin_lag7:himin_lag13,
         date_num, mo, wd)

tmax.hist.obs <- dat %>% select(tmax, tmax_lag1:tmax_lag6)
tmin.hist.obs <- dat %>% select(tmin, tmin_lag1:tmin_lag6)
himax.hist.obs <- dat %>% select(himax, himax_lag1:himax_lag6)
himin.hist.obs <- dat %>% select(himin, himin_lag1:himin_lag6)
cb.tmax.obs <- crossbasis(tmax.hist.obs, argvar = list(df=3), arglag = list(df=4))
cb.tmin.obs <- crossbasis(tmin.hist.obs, argvar = list(df=3), arglag = list(df=4))
cb.himax.obs <- crossbasis(himax.hist.obs, argvar = list(df=3), arglag = list(df=4))
cb.himin.obs <- crossbasis(himin.hist.obs, argvar = list(df=3), arglag = list(df=4))

make_long <- function(wide, nam){
  long <- wide %>% 
    pivot_longer(
      cols = "lag0":"lag6", 
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

res %>%
  mutate(m=ifelse(ref==0, "Minimum", "Maximum"),
         t_hi=ifelse(expo %in% c("Maximum Temperature (ref=8C)", "Minimum Temperature (ref=0C)"), "Temperature", "Heat index")) %>%
  filter((ref==0 & deg==19) | (ref!=0 & (deg %in% c(27, 32, 39)))) %>%
  mutate(deg=recode(deg,
                    "27" = "27C",
                    "32" = "32C",
                    "39" = "39C",
                    "19" = "19C")) %>%
  ggplot(aes(group=t_hi, color=t_hi)) +
  geom_hline(yintercept=1, linetype="dashed", color = "grey", linewidth=0.8) +
  # geom_vline(aes(xintercept=ref), linetype="dotted", color = "black", linewidth=0.8) +
  labs(title = this_lab, x="Lagged days", y="Relative Risk") +
  geom_ribbon(aes(x=lag, ymin=lo, ymax=hi, fill=t_hi), alpha=0.1, color=NA) +
  geom_line(aes(x=lag, y=est)) +
  scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) +
  facet_nested(m+deg~.) + theme_bw() + 
  ylim(0.95, 1.15) +
  theme(legend.title= element_blank(),
        legend.position="bottom")
#pdf 8x5 'sens_lag6'

###################################################################################
###################################################################################
## 14-day lag (Figure S2)
###################################################################################
###################################################################################

tmax.hist.obs <- dat %>% select(tmax, tmax_lag1:tmax_lag13)
tmin.hist.obs <- dat %>% select(tmin, tmin_lag1:tmin_lag13)
himax.hist.obs <- dat %>% select(himax, himax_lag1:himax_lag13)
himin.hist.obs <- dat %>% select(himin, himin_lag1:himin_lag13)
cb.tmax.obs <- crossbasis(tmax.hist.obs, argvar = list(df=3), arglag = list(df=8))
cb.tmin.obs <- crossbasis(tmin.hist.obs, argvar = list(df=3), arglag = list(df=8))
cb.himax.obs <- crossbasis(himax.hist.obs, argvar = list(df=3), arglag = list(df=8))
cb.himin.obs <- crossbasis(himin.hist.obs, argvar = list(df=3), arglag = list(df=8))

make_long <- function(wide, nam){
  long <- wide %>% 
    pivot_longer(
      cols = "lag0":"lag13", 
      names_to = "lag",
      values_to = nam
    )
  return(long)
}

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

res %>%
  mutate(m=ifelse(ref==0, "Minimum", "Maximum"),
         t_hi=ifelse(expo %in% c("Maximum Temperature (ref=8C)", "Minimum Temperature (ref=0C)"), "Temperature", "Heat index"),
         lag=factor(lag, levels=c("lag0", "lag1", "lag2", "lag3", "lag4", "lag5",
                                  "lag6", "lag7", "lag8", "lag9", "lag10", "lag11", "lag12", "lag13"))) %>%
  filter((ref==0 & deg==19) | (ref!=0 & (deg %in% c(27, 32, 39)))) %>%
  mutate(deg=recode(deg,
                    "27" = "27C",
                    "32" = "32C",
                    "39" = "39C",
                    "19" = "19C")) %>%
  ggplot(aes(group=t_hi, color=t_hi)) +
  geom_hline(yintercept=1, linetype="dashed", color = "grey", linewidth=0.8) +
  # geom_vline(aes(xintercept=ref), linetype="dotted", color = "black", linewidth=0.8) +
  labs(title = this_lab, x="Lagged days", y="Relative Risk") +
  geom_ribbon(aes(x=lag, ymin=lo, ymax=hi, fill=t_hi), alpha=0.1, color=NA) +
  geom_line(aes(x=lag, y=est)) +
  scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) +
  facet_nested(m+deg~.) + theme_bw() + 
  ylim(0.95, 1.15) +
  theme(legend.title= element_blank(),
        legend.position="bottom")
#pdf 8x10 'sens_lag13'

###################################################################################
###################################################################################
## same-day temperature and police calls: penalized splines (Figure S3)
###################################################################################
###################################################################################
mod1 <- gam(n ~ s(tmax) + ns(date_num, df=48) + as.factor(wd), family=quasipoisson, data=dat)
mod2 <- gam(n ~ s(tmin) + ns(date_num, df=48) + as.factor(wd), family=quasipoisson, data=dat)


pdf(file = paste("main/sens_penalized.pdf",sep = ""),   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 10) 
par(mfrow = c(2, 1))
plot(mod1)
plot(mod2)
dev.off()

###################################################################################
###################################################################################
## year*month (Figure S7)
###################################################################################
###################################################################################

t <- outcome %>%
  group_by(date_only) %>%
  summarise(n=n())

names(t) <- c("date", "n")

dat <- t %>%
  left_join(prism) %>%
  mutate(date_num=as.numeric(date)) %>%
  mutate(yr=year(date),
         mo=month(date)) %>%
  mutate(wd=weekdays(date)) %>%
  select(date, n, tmax, tmax_lag1:tmax_lag3, tmin, tmin_lag1:tmin_lag3, 
         himax, himax_lag1:himax_lag3, himin, himin_lag1:himin_lag3,
         date_num, yr, mo, wd)

tmax.hist.obs <- dat %>% select(tmax, tmax_lag1:tmax_lag3)
tmin.hist.obs <- dat %>% select(tmin, tmin_lag1:tmin_lag3)
himax.hist.obs <- dat %>% select(himax, himax_lag1:himax_lag3)
himin.hist.obs <- dat %>% select(himin, himin_lag1:himin_lag3)
cb.tmax.obs <- crossbasis(tmax.hist.obs, argvar = list(df=3), arglag = list(df=3))
cb.tmin.obs <- crossbasis(tmin.hist.obs, argvar = list(df=3), arglag = list(df=3))
cb.himax.obs <- crossbasis(himax.hist.obs, argvar = list(df=3), arglag = list(df=3))
cb.himin.obs <- crossbasis(himin.hist.obs, argvar = list(df=3), arglag = list(df=3))


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
mod_tmax <- glm(n ~ cb.tmax.obs + yr*mo + as.factor(wd), family=quasipoisson, data=dat)
mod_tmin <- glm(n ~ cb.tmin.obs + yr*mo + as.factor(wd), family=quasipoisson, data=dat)
mod_himax <- glm(n ~ cb.himax.obs + yr*mo + as.factor(wd), family=quasipoisson, data=dat)
mod_himin <- glm(n ~ cb.himin.obs + yr*mo + as.factor(wd), family=quasipoisson, data=dat)

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
  # geom_vline(aes(xintercept=ref), linetype="dotted", color = "black", linewidth=0.8) +
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
saveRDS(p_type, paste("parts\\", this, "_rr_sens_yrmo.rds",sep = ""))


## process res: Count Diff
###################################################################################
mod_tmax <- glm(n ~ cb.tmax.obs + yr*mo + as.factor(wd), family=quasipoisson(link = "identity"), data=dat)
mod_tmin <- glm(n ~ cb.tmin.obs + yr*mo + as.factor(wd), family=quasipoisson(link = "identity"), data=dat)
mod_himax <- glm(n ~ cb.himax.obs + yr*mo + as.factor(wd), family=quasipoisson(link = "identity"), data=dat)
mod_himin <- glm(n ~ cb.himin.obs + yr*mo + as.factor(wd), family=quasipoisson(link = "identity"), data=dat)

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
  # geom_vline(aes(xintercept=ref), linetype="dotted", color = "black", linewidth=0.8) +
  labs(title = this_lab, x="Celcius", y="Count Difference") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi, fill=t_hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) +
  facet_nested(.~m+lag) + theme_bw() + 
  theme(legend.title= element_blank(),
        legend.position="bottom")

# save subplot into new folder named 'parts' to be combined later for supplemental figures
saveRDS(p_type, paste("parts\\", this, "_cd_sens_yrmo.rds",sep = ""))


###################################################################################
###################################################################################
## increase df of lag-response cross basis df=3-->df=2 (Figure S8)
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
  mutate(wd=weekdays(date)) %>%
  select(date, n, tmax, tmax_lag1:tmax_lag3, tmin, tmin_lag1:tmin_lag3, 
         himax, himax_lag1:himax_lag3, himin, himin_lag1:himin_lag3,
         date_num, mo, wd)

tmax.hist.obs <- dat %>% select(tmax, tmax_lag1:tmax_lag3)
tmin.hist.obs <- dat %>% select(tmin, tmin_lag1:tmin_lag3)
himax.hist.obs <- dat %>% select(himax, himax_lag1:himax_lag3)
himin.hist.obs <- dat %>% select(himin, himin_lag1:himin_lag3)
cb.tmax.obs <- crossbasis(tmax.hist.obs, argvar = list(df=3), arglag = list(df=2))
cb.tmin.obs <- crossbasis(tmin.hist.obs, argvar = list(df=3), arglag = list(df=2))
cb.himax.obs <- crossbasis(himax.hist.obs, argvar = list(df=3), arglag = list(df=2))
cb.himin.obs <- crossbasis(himin.hist.obs, argvar = list(df=3), arglag = list(df=2))

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
  # geom_vline(aes(xintercept=ref), linetype="dotted", color = "black", linewidth=0.8) +
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
saveRDS(p_type, paste("parts\\", this, "_rr_sens_df2.rds",sep = ""))


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
  # geom_vline(aes(xintercept=ref), linetype="dotted", color = "black", linewidth=0.8) +
  labs(title = this_lab, x="Celcius", y="Count Difference") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi, fill=t_hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) +
  facet_nested(.~m+lag) + theme_bw() + 
  theme(legend.title= element_blank(),
        legend.position="bottom")

# save subplot into new folder named 'parts' to be combined later for supplemental figures
saveRDS(p_type, paste("parts\\", this, "_cd_sens_df2.rds",sep = ""))


###################################################################################
###################################################################################
## increase df of cross basis df=3-->df=4 (Figure S9)
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
  mutate(wd=weekdays(date)) %>%
  select(date, n, tmax, tmax_lag1:tmax_lag3, tmin, tmin_lag1:tmin_lag3, 
         himax, himax_lag1:himax_lag3, himin, himin_lag1:himin_lag3,
         date_num, mo, wd)

tmax.hist.obs <- dat %>% select(tmax, tmax_lag1:tmax_lag3)
tmin.hist.obs <- dat %>% select(tmin, tmin_lag1:tmin_lag3)
himax.hist.obs <- dat %>% select(himax, himax_lag1:himax_lag3)
himin.hist.obs <- dat %>% select(himin, himin_lag1:himin_lag3)
cb.tmax.obs <- crossbasis(tmax.hist.obs, argvar = list(df=4), arglag = list(df=4))
cb.tmin.obs <- crossbasis(tmin.hist.obs, argvar = list(df=4), arglag = list(df=4))
cb.himax.obs <- crossbasis(himax.hist.obs, argvar = list(df=4), arglag = list(df=4))
cb.himin.obs <- crossbasis(himin.hist.obs, argvar = list(df=4), arglag = list(df=4))

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
  # geom_vline(aes(xintercept=ref), linetype="dotted", color = "black", linewidth=0.8) +
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
saveRDS(p_type, paste("parts\\", this, "_rr_sens_df4.rds",sep = ""))


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
  # geom_vline(aes(xintercept=ref), linetype="dotted", color = "black", linewidth=0.8) +
  labs(title = this_lab, x="Celcius", y="Count Difference") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi, fill=t_hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) +
  facet_nested(.~m+lag) + theme_bw() + 
  theme(legend.title= element_blank(),
        legend.position="bottom")

# save subplot into new folder named 'parts' to be combined later for supplemental figures
saveRDS(p_type, paste("parts\\", this, "_cd_sens_df4.rds",sep = ""))