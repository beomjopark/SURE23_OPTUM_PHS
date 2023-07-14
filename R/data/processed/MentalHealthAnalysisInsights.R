library(tidyverse)
library(ggplot2)
#read our cleaned up data into the env
data_analytic<- readRDS("../test/data/interim/analytic2023_c1.rds")
desc <- readRDS("data/processed/colDesc_analysis2023.rds")

v005_race_vars <- c("v005_race_hispanic","v005_race_white","v005_race_asian","v005_race_aian","v005_race_black")

v138_race_vars <- c("v138_race_hispanic", "v138_race_white", "v138_race_asian", "v138_race_aian", "v138_race_black")

v063_race_vars <- c("v063_race_hispanic", "v063_race_white", "v063_race_asian", "v063_race_aian", "v063_race_black")

#Preventable Hospital stays(v005): Rate of hospital stays for ambulatory-care sensitive conditions per 100,000 Medicare enrollees.

#Drug overdose (v138): Number of drug poisoning deaths per 100,000 population.

#Median House Hold Income(v063): The income where half of households in a county earn more and half of households earn less.

#MHD(v042_rawvalue): Average number of mentally unhealthy days reported in past 30 days (age-adjusted).

#FMD(v145):Percentage of adults reporting 14 or more days of poor mental health per month (age-adjusted).

#PFH(v002): Percentage of adults reporting fair or poor health (age-adjusted).


ggplot(data_analytic, aes(y = log(v005_rawvalue), x = log(v042_rawvalue)))+
  geom_point(color="green", alpha =0.5)+
  geom_smooth()+
  xlab(desc[["v042_rawvalue"]]) +
  ylab(desc[["v005_rawvalue"]])+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))

ggplot(data_analytic, aes(y = log(v005_rawvalue), x = log(v145_rawvalue)))+
  geom_point(color="green",alpha =0.5)+
  geom_smooth() +
  
  xlab(desc[["v145_rawvalue"]]) +
  ylab(desc[["v005_rawvalue"]])+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))

ggplot(data_analytic, aes(y = log(v005_rawvalue), x = log(v002_rawvalue)))+
  geom_point(color="green", alpha =0.5)+
  geom_smooth()+
  xlab(desc[["v002_rawvalue"]]) +
  ylab(desc[["v005_rawvalue"]])+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))


for( var in v005_race_vars){
  v002byraceVsPMHD <- ggplot(data_analytic, aes_string(y = paste0("log(",var,")"), x = "(v002_rawvalue)"))+
    geom_point(color="green", alpha =0.5)+
    geom_smooth()+
    xlab(desc[["v002_rawvalue"]]) +
    ylab(desc[[var]])+
    theme(panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "white", size = 0.1),
          panel.grid.minor = element_line(color = "white", size = 0.05),
          text = element_text(color = "white"))
  print(v002byraceVsPMHD)
}

ggplot(data_analytic, aes(y = log(v005_rawvalue), x = log(v138_rawvalue)))+
  geom_point(color="green", alpha =0.5)+
  geom_smooth()+
  xlab(desc[["v138_rawvalue"]]) +
  ylab(desc[["v005_rawvalue"]])+
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))

for(i in 1:length(v138_race_vars)){
  var_x <- v138_race_vars[i]
  var_y <- v005_race_vars[i]
  
  v138byracegraph <-ggplot(data_analytic, aes_string(y = var_y, x = var_x))+
    geom_point(color="green", alpha =0.5)+
    geom_smooth()+
    xlab(desc[[var_x]]) +
    ylab(desc[[var_y]])+
    theme(panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "white", size = 0.1),
          panel.grid.minor = element_line(color = "white", size = 0.05),
          text = element_text(color = "white")) 
  print(v138byracegraph)
}

for(i in 1:length(v063_race_vars)){
  var_x <- v063_race_vars[i]
  var_y <- v005_race_vars[i]
  
  v063byracegraph <-ggplot(data_analytic, aes_string(y = var_y, x = var_x))+
    geom_point(color="green", alpha =0.5)+
    geom_smooth()+
    xlab(desc[[var_x]]) +
    ylab(desc[[var_y]])+
    theme(panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "white", size = 0.1),
          panel.grid.minor = element_line(color = "white", size = 0.05),
          text = element_text(color = "white")) 
  print(v063byracegraph)
}

