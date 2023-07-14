library(tidyverse)
library(ggplot2)
#read our cleaned up data into the env
data_analytic<- readRDS("data/interim/analytic2023_c1.rds")

#Injury deaths Raw(v135):Number of deaths due to injury per 100,000 population.
ggplot(data_analytic, aes(y = v005_rawvalue, x = v135_rawvalue))+
  geom_point(color="green", alpha =0.5)+
  geom_smooth(method="lm", col="red") +
  geom_smooth()+
  xlab(paste("Number of deaths due to injury per 100,000 population")) +
  ylab("Preventable Hospital Stays")+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))

#Suicide raw(v161):Number of deaths due to suicide per 100,000 population (age-adjusted).
ggplot(data_analytic, aes(y = v005_rawvalue, x = v161_rawvalue))+
  geom_point(color="green", alpha =0.5)+
  geom_smooth()+
  xlab(paste("Number of deaths due to injury per 100,000 population")) +
  ylab("Preventable Hospital Stays")+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))

#fire arm deaths raw(v148):Number of deaths due to firearms per 100,000 population.
ggplot(data_analytic, aes(y = v005_rawvalue, x = v148_rawvalue))+
  geom_point(color="green", alpha =0.5)+
  geom_smooth()+
  xlab(paste("Number of deaths due to injury per 100,000 population")) +
  ylab("Preventable Hospital Stays")+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))
