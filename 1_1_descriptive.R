#####################################
#####################################
###### Prepare/clean dataset ########
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
library(ggrepel)
library(table1)

# load in derived datasets (from 1_0_prep rds file)
prism <- readRDS("prism.rds")
outcome <- readRDS("incident_key.rds")

###################################################################################
###################################################################################
## Table 1
###################################################################################
###################################################################################
t <- outcome %>%
  group_by(date_only) %>%
  summarise(n=n())

names(t) <- c("date", "n")

dat <- t %>%
  left_join(prism) %>%
  mutate(date_num=as.numeric(date)) %>%
  mutate(mo=month(date)) 
  # filter(!(mo %in% 5:10))

quantile(dat$tmax, probs = c(0.5, 0.75, 0.9))
quantile(dat$tmin, probs = c(0.5, 0.75, 0.9))

table1(~ n + tmax + tmin + himax + himin, data=dat)

mean(dat$n)
sd(dat$n)

mean(dat$tmax)
sd(dat$tmax)

mean(dat$tmin)
sd(dat$tmin)

mean(dat$himax)
sd(dat$himax)

mean(dat$himin)
sd(dat$himin)

t <- outcome %>%
  # mutate(group = recode(group, "health_mental"="health_non")) %>%
  mutate(date_num=as.numeric(date_only)) %>%
  mutate(mo=month(date_only)) %>%
  # filter(mo %in% 5:10) %>%
  filter(!(mo %in% 5:10)) %>%
  group_by(date_only, group) %>%
  summarise(n=n()) %>%
  ungroup() %>% 
  group_by(group) %>%
  summarise(m=round(mean(n),1),
            s=round(sd(n),1))

###################################################################################
###################################################################################
### TEMPORAL TRENDS IN TEMPERATURE AND POLICE CALLS (Figure 1)
###################################################################################
###################################################################################

df_d <- dat %>%
  select(date, n, tmax, tmin, himax, himin) %>%
  pivot_longer(
    cols = c("n", "tmax", "tmin", "himax", "himin"), # Selects columns 
    names_to = "var",   # Name of the new column to store the old column names 
    values_to = "val"         # Name of the new column to store the values
  ) %>%
  mutate() %>%
  mutate(gr=ifelse(var %in% c("tmax", "himax"), "Maximum",
                   ifelse(var %in% c("tmin", "himin"), "Minimum", var)),
         gr=recode(gr,
                   'n' = 'Dispatched calls'),
         gr=factor(gr, levels = c('Maximum', 'Minimum', 'Dispatched calls')),
         var = recode(var,
                      'n' = 'Police calls',
                      'tmax' = 'Temperature',
                      'tmin' = 'Temperature',
                      'himax' = 'Heat index',
                      'himin' = 'Heat index'
         ))


end <- df_d %>%
  group_by(var) %>%
  filter(date == max(date))


df_d %>%
  ggplot(aes(x=date, y=val, color=var)) +
  geom_line() +
  geom_text_repel(data=end, aes(label=var),
                   nudge_x=10, # Adjust positioning
                   segment.color = 'grey', # Color of the connecting lines
                   show.legend = FALSE,
                  size=3) +
  scale_color_manual(values = c('black', 'black', 'grey65')) +
  facet_grid(gr~., scales="free_y") +
  expand_limits(x=max(dat$date) + 100) + # Expand the x-axis limits
  theme_bw() +
  theme(legend.position = "none") +
  labs(x="Calendar date", y="Celsius for temperature or count for calls")


###################################################################################
###################################################################################
### DISTRIBUTION OF EXPOSURE (Figure S4)
###################################################################################
###################################################################################

dat %>%
  na.omit() %>%
  select(tmax, tmin, himax, himin) %>%
  pivot_longer(
    cols = c("tmax", "tmin", "himax", "himin"), 
    names_to = "var",   
    values_to = "val"         # Name of the new column to store the values
  ) %>%
  mutate(var=recode(var,
                    "himax"="Maximum heat index",
                    "tmax"="Maximum temperature",
                    "himin"="Minimum heat index",
                    "tmin"="Minimum temperature"),
         var=factor(var, levels = c("Maximum temperature", "Maximum heat index",
                                    "Minimum temperature", "Minimum heat index"))) %>%
  ggplot() +
  geom_density(aes(val)) +
  facet_grid(var~.) +
  labs(x="Celsius", y="Density") +
  scale_x_continuous(breaks = seq(-10, 50, 5)) +
  theme_bw()
