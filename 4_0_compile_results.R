
rm(list = ls())


setwd("")

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggh4x)

###################################################################################
###################################################################################
## prep
###################################################################################
###################################################################################
reason_group <- c("call", "violent", "nonviolent", "vehicle", "domestic", "health", "sex", "other")

d <- rbind(readRDS(paste("...main\\", reason_group[1], "_rr.rds",sep = "")),
           readRDS(paste("...main\\", reason_group[2], "_rr.rds",sep = "")),
           readRDS(paste("...main\\", reason_group[3], "_rr.rds",sep = "")),
           readRDS(paste("...main\\", reason_group[4], "_rr.rds",sep = "")),
           readRDS(paste("...main\\", reason_group[5], "_rr.rds",sep = "")),
           readRDS(paste("...main\\", reason_group[6], "_rr.rds",sep = "")),
           readRDS(paste("...main\\", reason_group[7], "_rr.rds",sep = "")),
           readRDS(paste("...main\\", reason_group[8], "_rr.rds",sep = "")))

d1 <- rbind(readRDS(paste("...main\\", reason_group[1], "_cd.rds",sep = "")),
            readRDS(paste("...main\\", reason_group[2], "_cd.rds",sep = "")),
            readRDS(paste("...main\\", reason_group[3], "_cd.rds",sep = "")),
            readRDS(paste("...main\\", reason_group[4], "_cd.rds",sep = "")),
            readRDS(paste("...main\\", reason_group[5], "_cd.rds",sep = "")),
            readRDS(paste("...main\\", reason_group[6], "_cd.rds",sep = "")),
            readRDS(paste("...main\\", reason_group[7], "_cd.rds",sep = "")),
            readRDS(paste("...main\\", reason_group[8], "_cd.rds",sep = "")))

fig1a <- d %>%
  mutate(type=recode(type,
                     "violent" = "Violent",
                     "nonviolent" = "Nonviolent",
                     "vehicle" = "Road",
                     "domestic" = "Domestic",
                     "health" = "Health",
                     "sex" = "Sexual",
                     "other" = "Other"),
         type=factor(type, levels=c("Overall", "Nonviolent", "Violent", "Road", "Domestic", "Health", "Sexual", "Other")),
         m=ifelse(ref==0, "Minimum", "Maximum"),
         t_hi=ifelse(expo %in% c("Maximum Temperature (ref=8C)", "Minimum Temperature (ref=0C)"), "Temperature", "Heat index")) %>%
  filter(t_hi == "Temperature",
         m=="Maximum") %>%
  ggplot() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey", linewidth=0.8) +
  labs(title = "Maximum temperature", x="Celsius", y="Relative Risk (refernce=8C)") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  facet_nested(lag~type) + theme_test() + 
  scale_y_continuous(breaks=seq(0.8,1.5,.1), limits = c(0.8, 1.5))

fig1b <- d1 %>%
  mutate(type=recode(type,
                     "violent" = "Violent",
                     "nonviolent" = "Nonviolent",
                     "vehicle" = "Road",
                     "domestic" = "Domestic",
                     "health" = "Health",
                     "sex" = "Sexual",
                     "other" = "Other"),
         type=factor(type, levels=c("Overall", "Nonviolent", "Violent", "Road", "Domestic", "Health", "Sexual", "Other")),
         m=ifelse(ref==0, "Minimum", "Maximum"),
         t_hi=ifelse(expo %in% c("Maximum Temperature (ref=8C)", "Minimum Temperature (ref=0C)"), "Temperature", "Heat index")) %>%
  filter(t_hi == "Temperature",
         m=="Maximum") %>%
  ggplot() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey", linewidth=0.8) +
  labs(x="Celsius", y="Count difference (ref=8C)") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  facet_nested(lag~type) + theme_test() + 
  scale_y_continuous(breaks=seq(-50,250,25), limits = c(-50, 250))

pdf(file = paste("...main/fig1ab.pdf",sep = ""),   # The directory you want to save the file in
    width = 9, # The width of the plot in inches
    height = 10) 

grid.arrange(fig1a, fig1b, ncol=1) 

dev.off()

####################

fig2a <- d %>%
  mutate(type=recode(type,
                     "violent" = "Violent",
                     "nonviolent" = "Nonviolent",
                     "vehicle" = "Road",
                     "domestic" = "Domestic",
                     "health" = "Health",
                     "sex" = "Sexual",
                     "other" = "Other"),
         type=factor(type, levels=c("Overall", "Nonviolent", "Violent", "Road", "Domestic", "Health", "Sexual", "Other")),
         m=ifelse(ref==0, "Minimum", "Maximum"),
         t_hi=ifelse(expo %in% c("Minimum Temperature (ref=0C)", "Minimum Temperature (ref=0C)"), "Temperature", "Heat index")) %>%
  filter(t_hi == "Temperature",
         m=="Minimum") %>%
  ggplot() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey", linewidth=0.8) +
  labs(title = "Minimum temperature", x="Celsius", y="Relative Risk (refernce=0C)") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  facet_nested(lag~type) + theme_test() + 
  scale_y_continuous(breaks=seq(0.8,1.5,.1), limits = c(0.8, 1.5))

fig2b <- d1 %>%
  mutate(type=recode(type,
                     "violent" = "Violent",
                     "nonviolent" = "Nonviolent",
                     "vehicle" = "Road",
                     "domestic" = "Domestic",
                     "health" = "Health",
                     "sex" = "Sexual",
                     "other" = "Other"),
         type=factor(type, levels=c("Overall", "Nonviolent", "Violent", "Road", "Domestic", "Health", "Sexual", "Other")),
         m=ifelse(ref==0, "Minimum", "Maximum"),
         t_hi=ifelse(expo %in% c("Maximum Temperature (ref=8C)", "Minimum Temperature (ref=0C)"), "Temperature", "Heat index")) %>%
  filter(t_hi == "Temperature",
         m=="Minimum") %>%
  ggplot() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey", linewidth=0.8) +
  labs(x="Celsius", y="Count difference (ref=0C)") +
  geom_ribbon(aes(x=deg, ymin=lo, ymax=hi), alpha=0.1, color=NA) +
  geom_line(aes(x=deg, y=est)) +
  facet_nested(lag~type) + theme_test() + 
  ylim(-50, 175) +
  scale_y_continuous(breaks=seq(-50,250,25), limits = c(-50, 250))



pdf(file = paste("...main/fig2ab.pdf",sep = ""),   # The directory you want to save the file in
    width = 9, # The width of the plot in inches
    height = 10) 
grid.arrange(fig2a, fig2b, ncol=1) 
dev.off()

####################
####################

# Fig S1: RR
p1 <- readRDS(paste("...parts\\", reason_group[1], "_rr.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + ylab("Relative Risk") + theme(text = element_text(size = 15))
p2 <- readRDS(paste("...parts\\", reason_group[2], "_rr.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + ylab("Relative Risk") + theme(text = element_text(size = 15)) 
p3 <- readRDS(paste("...parts\\", reason_group[3], "_rr.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + ylab("Relative Risk") + theme(text = element_text(size = 15))
p4 <- readRDS(paste("...parts\\", reason_group[4], "_rr.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + ylab("Relative Risk") + theme(text = element_text(size = 15))
p5 <- readRDS(paste("...parts\\", reason_group[5], "_rr.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + ylab("Relative Risk") + theme(text = element_text(size = 15))
p6 <- readRDS(paste("...parts\\", reason_group[6], "_rr.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + ylab("Relative Risk") + theme(text = element_text(size = 15))
p7 <- readRDS(paste("...parts\\", reason_group[7], "_rr.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + ylab("Relative Risk") + theme(text = element_text(size = 15))
p8 <- readRDS(paste("...parts\\", reason_group[8], "_rr.rds",sep = "")) + xlab("Celsius") + ylab("Relative Risk") + theme(text = element_text(size = 15))

pdf(file = paste("...main/figs1.pdf",sep = ""),   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 30) 
grid.arrange(p1, p3, p2, p4, p5, p6, p7, p8, ncol=1) 
dev.off()

##

prism <- readRDS("prism.rds")
outcome <- readRDS("incident_key.rds")

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
# Fig S2: CD
p1 <- readRDS(paste("...parts\\", reason_group[1], "_cd.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + theme(text = element_text(size = 15))
p2 <- readRDS(paste("...parts\\", reason_group[2], "_cd.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + theme(text = element_text(size = 15))
p3 <- readRDS(paste("...parts\\", reason_group[3], "_cd.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + theme(text = element_text(size = 15))
p4 <- readRDS(paste("...parts\\", reason_group[4], "_cd.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + theme(text = element_text(size = 15))
p5 <- readRDS(paste("...parts\\", reason_group[5], "_cd.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + theme(text = element_text(size = 15))
p6 <- readRDS(paste("...parts\\", reason_group[6], "_cd.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + theme(text = element_text(size = 15))
p7 <- readRDS(paste("...parts\\", reason_group[7], "_cd.rds",sep = "")) +theme(legend.position="none") + xlab("Celsius") + theme(text = element_text(size = 15))
p8 <- readRDS(paste("...parts\\", reason_group[8], "_cd.rds",sep = "")) + xlab("Celsius") + theme(text = element_text(size = 15))

pdf(file = paste("...main/figs2.pdf",sep = ""),   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 30) 
grid.arrange(p1, p3, p2, p4, p5, p6, p7, p8, ncol=1) 
dev.off()

####################
####################

# Fig S3: RR warm/cold
p1 <- readRDS("...parts\\call_warm_rr.rds")+theme(legend.position="none") +ylim(0.9,1.2)
p2 <- readRDS("...parts\\call_cold_rr.rds") +ylim(0.9,1.2)

pdf(file = paste("...main/figs3.pdf",sep = ""),   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 10) 
grid.arrange(p1, p2, ncol=1) 
dev.off()

# Fig S4: CD warm/cold
p1 <- readRDS("...parts\\call_warm_cd.rds")+theme(legend.position="none")
p2 <- readRDS("...parts\\call_cold_cd.rds")

pdf(file = paste("...main/figs4.pdf",sep = ""),   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 10) 
grid.arrange(p1, p2, ncol=1) 
dev.off()


####################
####################

# Fig S5: health rr
p1 <- readRDS("...parts\\health_mental_rr.rds")+theme(legend.position="none") + xlab("Celsius") + ylim(0.8, 2.2)
p2 <- readRDS("...parts\\health_non_rr.rds")  + xlab("Celsius") + ylim(0.8, 2.2)

pdf(file = paste("...main/figs5.pdf",sep = ""),   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 10) 
grid.arrange(p1, p2, ncol=1) 
dev.off()

# Fig S6: health cd
p1 <- readRDS("...parts\\health_mental_cd.rds")+theme(legend.position="none") + xlab("Celsius") 
p2 <- readRDS("...parts\\health_non_cd.rds")  + xlab("Celsius") 

pdf(file = paste("...main/figs6.pdf",sep = ""),   # The directory you want to save the file in
    width = 15, # The width of the plot in inches
    height = 10) 
grid.arrange(p1, p2, ncol=1) 
dev.off()