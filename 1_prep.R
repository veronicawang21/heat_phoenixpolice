
setwd("")

library(tidyverse)
library(stringr)
library(ggeffects)
library(ggplot2)
library(weathermetrics) 
library(splines)
library(mgcv)

# load PRISM exposure
prism_tmax <- read.csv("...Phoenix_PopWeightPRISM\\SSC_PhoenixPRISMDAILY_LONGtmax.csv")
prism_vapmax <- read.csv("...Phoenix_PopWeightPRISM\\SSC_PhoenixPRISMDAILY_LONGvpdmax.csv") %>% select(vpdmax)
prism_tmin <- read.csv("...Phoenix_PopWeightPRISM\\SSC_PhoenixPRISMDAILY_LONGtmin.csv") %>% select(tmin)
prism_vapmin <- read.csv("...Phoenix_PopWeightPRISM\\SSC_PhoenixPRISMDAILY_LONGvpdmin.csv") %>% select(vpdmin)

#vapd: vapor pressure deficit

prism <- prism_tmax %>%
  mutate(date=seq(as.Date('2014-01-01'),as.Date('2023-12-31'), 'day')) %>%
  select(date, tmax) %>%
  cbind(prism_vapmax, prism_tmin, prism_vapmin) %>%
  mutate(satvp_max=6.11*exp((17.62 * tmax) / (243.12 + tmax)),  # using August-Roche-Magnus equation
         satvp_min=6.11*exp((17.62 * tmin) / (243.12 + tmin)),
         vp_max=satvp_max-vpdmax,
         vp_min=satvp_min-vpdmin,
         rh_max=100*(vp_max/satvp_max),
         rh_min=100*(vp_min/satvp_min),
         himax=heat.index(t=tmax, rh=rh_max, temperature.metric = 'celsius'),
         himin=heat.index(t=tmin, rh=rh_min, temperature.metric = 'celsius'))

prism <- prism %>% filter(date>="2016-01-01") %>%
  mutate(tmax_lag1=lag(tmax, n=1),
         tmax_lag2=lag(tmax, n=2),
         tmax_lag3=lag(tmax, n=3),
         tmax_lag4=lag(tmax, n=4),
         tmax_lag5=lag(tmax, n=5),
         tmax_lag6=lag(tmax, n=6),
         tmin_lag1=lag(tmin, n=1),
         tmin_lag2=lag(tmin, n=2),
         tmin_lag3=lag(tmin, n=3),
         tmin_lag4=lag(tmin, n=4),
         tmin_lag5=lag(tmin, n=5),
         tmin_lag6=lag(tmin, n=6),
         himax_lag1=lag(himax, n=1),
         himax_lag2=lag(himax, n=2),
         himax_lag3=lag(himax, n=3),
         himax_lag4=lag(himax, n=4),
         himax_lag5=lag(himax, n=5),
         himax_lag6=lag(himax, n=6),
         himin_lag1=lag(himin, n=1),
         himin_lag2=lag(himin, n=2),
         himin_lag3=lag(himin, n=3),
         himin_lag4=lag(himin, n=4),
         himin_lag5=lag(himin, n=5),
         himin_lag6=lag(himin, n=6)) %>%
  rowwise() %>%
  mutate(tmax_mov1=mean(c_across(c("tmax","tmax_lag1")), na.rm = T),
         tmax_mov2=mean(c_across(c("tmax","tmax_lag1","tmax_lag2")), na.rm = T),
         tmax_mov3=mean(c_across(c("tmax","tmax_lag1","tmax_lag2","tmax_lag3")), na.rm = T),
         tmax_mov4=mean(c_across(c("tmax","tmax_lag1","tmax_lag2","tmax_lag3","tmax_lag4")), na.rm = T),
         tmax_mov5=mean(c_across(c("tmax","tmax_lag1","tmax_lag2","tmax_lag3","tmax_lag4","tmax_lag5")), na.rm = T),
         tmax_mov6=mean(c_across(c("tmax","tmax_lag1","tmax_lag2","tmax_lag3","tmax_lag4","tmax_lag5","tmax_lag6")), na.rm = T),
         tmin_mov1=mean(c_across(c("tmin","tmin_lag1")), na.rm = T),
         tmin_mov2=mean(c_across(c("tmin","tmin_lag1","tmin_lag2")), na.rm = T),
         tmin_mov3=mean(c_across(c("tmin","tmin_lag1","tmin_lag2","tmin_lag3")), na.rm = T),
         tmin_mov4=mean(c_across(c("tmin","tmin_lag1","tmin_lag2","tmin_lag3","tmin_lag4")), na.rm = T),
         tmin_mov5=mean(c_across(c("tmin","tmin_lag1","tmin_lag2","tmin_lag3","tmin_lag4","tmin_lag5")), na.rm = T),
         tmin_mov6=mean(c_across(c("tmin","tmin_lag1","tmin_lag2","tmin_lag3","tmin_lag4","tmin_lag5","tmin_lag6")), na.rm = T),
         himax_mov1=mean(c_across(c("himax","himax_lag1")), na.rm = T),
         himax_mov2=mean(c_across(c("himax","himax_lag1","himax_lag2")), na.rm = T),
         himax_mov3=mean(c_across(c("himax","himax_lag1","himax_lag2","himax_lag3")), na.rm = T),
         himax_mov4=mean(c_across(c("himax","himax_lag1","himax_lag2","himax_lag3","himax_lag4")), na.rm = T),
         himax_mov5=mean(c_across(c("himax","himax_lag1","himax_lag2","himax_lag3","himax_lag4","himax_lag5")), na.rm = T),
         himax_mov6=mean(c_across(c("himax","himax_lag1","himax_lag2","himax_lag3","himax_lag4","himax_lag5","himax_lag6")), na.rm = T),
         himin_mov1=mean(c_across(c("himin","himin_lag1")), na.rm = T),
         himin_mov2=mean(c_across(c("himin","himin_lag1","himin_lag2")), na.rm = T),
         himin_mov3=mean(c_across(c("himin","himin_lag1","himin_lag2","himin_lag3")), na.rm = T),
         himin_mov4=mean(c_across(c("himin","himin_lag1","himin_lag2","himin_lag3","himin_lag4")), na.rm = T),
         himin_mov5=mean(c_across(c("himin","himin_lag1","himin_lag2","himin_lag3","himin_lag4","himin_lag5")), na.rm = T),
         himin_mov6=mean(c_across(c("himin","himin_lag1","himin_lag2","himin_lag3","himin_lag4","himin_lag5","himin_lag6")), na.rm = T))

# saveRDS(prism, "prism.rds")

# clean outcome data
df <- read.csv("pheonix_police_og.csv")
group_key <- read.csv("phoenix_grouping.csv")

df_group <- df %>%
  left_join(group_key)

names(df_group)[names(df_group) == "CALL_RECEIVED"] <- "call_date"

df1 <- df_group %>%
  mutate(call_date=str_replace_all(call_date, " ", ""),
         call_date=strptime(call_date, format =  "%m/%d/%Y%H:%M:%S%p"),
         date_only=as.Date(call_date)) %>%
  select(date_only, group) %>%
  filter(date_only >= "2016-01-01", date_only < "2024-01-01") %>%
  filter(group!='exclude')

#saveRDS(df1, "incident_key.rds")
