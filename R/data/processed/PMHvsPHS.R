library(tidyverse)
library(ggplot2)
#read our cleaned up data into the env
data_analytic<- readRDS("data/interim/analytic2023_c1.rds")

#we want to plot poor mental health days and PHS
#PMHD(v042_rawvalue):Average number of mentally unhealthy days reported in past 30 days (age-adjusted).
ggplot(data_analytic, aes(y = v005_rawvalue, x = v042_rawvalue))+
  geom_point(color="green", alpha =0.5)+
  geom_smooth()+
  xlab(paste("Average number of mentally unhealthy days reported in past 30 days (age-adjusted).")) +
  ylab("Preventable Hospital Stays")+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))

#phs vs frequent mental distress
#FMD(v145):Percentage of adults reporting 14 or more days of poor mental health per month (age-adjusted).
ggplot(data_analytic, aes(y = v005_rawvalue, x = v145_rawvalue))+
  geom_point(color="green",alpha =0.5)+
  geom_smooth() +
  xlab(paste("Ratio of household income at the 80th percentile to income at the 20th percentile.")) +
  ylab("Preventable Hospital Stays") +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))

#phs vs poor or fair health 
#PFH(v002):Percentage of adults reporting fair or poor health (age-adjusted).
ggplot(data_analytic, aes(y = v005_rawvalue, x = v002_rawvalue))+
  geom_point(color="green", alpha =0.5)+
  geom_smooth()+
  xlab(paste("Percentage of adults reporting fair or poor health (age-adjusted)")) +
  ylab("Preventable Hospital Stays") +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))

