library(tidyverse)
library(ggplot2)
#read our cleaned up data into the env
data_analytic<- readRDS("data/interim/analytic2023_c1.rds")
data_phs_raw <- data_analytic %>% select("v005_rawvalue")
data_pmh_raw <-data_analytic %>% select("v024_rawvalue")
#we want to plot poor mental health and PHS
ggplot(data_analytic, aes(x = v005_rawvalue, y = v024_rawvalue))+
  geom_point(color="white")+
  theme(panel.background = element_rect(fill = "black"),
           plot.background = element_rect(fill = "black"),
           panel.grid.major = element_line(color = "white", size = 0.1),
           panel.grid.minor = element_line(color = "white", size = 0.05),
           text = element_text(color = "white"))

