library(tidyverse)
library(ggplot2)
#import Data
data_analytic<- readRDS("../test/data/interim/analytic2023_c1.rds")
#impoort Desc Dictionary
desc <- readRDS("../test/data/processed/colDesc_analysis2023.rds")

#gather race vars 
v067_race_vars <- c("v067_race_hispanic", "v067_race_white", "v067_race_asian", "v067_race_aian", "v067_race_black")
v138_race_vars <- c("v138_race_hispanic", "v138_race_white", "v138_race_asian", "v138_race_aian", "v138_race_black")
v005_race_vars<- c("v005_race_hispanic","v005_race_white","v005_race_asian","v005_race_aian","v005_race_black")
v161_race_vars <- c("v161_race_hispanic", "v161_race_white", "v161_race_asian", "v161_race_aian", "v161_race_black")
v039_race_vars <- c("v039_race_hispanic", "v039_race_white", "v039_race_asian", "v039_race_aian", "v039_race_black")

for( var in v067_race_vars){
  PHSrawVsDriveAlonetoworkbyrace <- ggplot(data_analytic, aes_string(y = paste0("log(",var,")"), x = "v005_rawvalue"))+
    geom_point(color="green", alpha =0.5)+
    geom_smooth()+
    xlab(desc[["v145_rawvalue"]]) +
    ylab(desc[[var]])+
    theme(panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "white", size = 0.1),
          panel.grid.minor = element_line(color = "white", size = 0.05),
          text = element_text(color = "white"))
  print(  PHSrawVsDriveAlonetoworkbyrace)
}

for(i in 1:length(v138_race_vars )){
  var_x <- v067_race_vars[i]
  var_y <- v138_race_vars[i]
  
  longCommsVSDrugs <-ggplot(data_analytic, aes_string(y = paste0("log(1+",var_y, ")"), x = paste0("log(1+",var_x,")")))+
    geom_point(color="green", alpha =0.5)+
    geom_smooth()+
    xlab(desc[[var_x]]) +
    ylab(desc[[var_y]])+
    theme(panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "white", size = 0.1),
          panel.grid.minor = element_line(color = "white", size = 0.05),
          text = element_text(color = "white"))
  print(longCommsVSDrugs)
}

for(i in 1:length(v138_race_vars )){
  var_x <- v067_race_vars[i]
  var_y <- v138_race_vars[i]
  
  longCommsVSDrugs <-ggplot(data_analytic, aes_string(y = paste0("log(1+",var_y, ")"), x = paste0("log( 1+",var_x,")")))+
    geom_point(color="green", alpha =0.5)+
    geom_smooth()+
    xlab(desc[[var_x]]) +
    ylab(desc[[var_y]])+
    theme(panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "white", size = 0.1),
          panel.grid.minor = element_line(color = "white", size = 0.05),
          text = element_text(color = "white"))
  print(longCommsVSDrugs)
}

for(i in 1:length(v067_race_vars )){
  var_x <- v067_race_vars[i] 
  var_y <- v039_race_vars[i]
  suicideVsCardeaths <-ggplot(data_analytic, aes_string(y = paste0("log(1+",var_y, ")"), x = paste0("log( 1+", var_x,")"))) +
    geom_point(color="green", alpha =0.5)+
    geom_smooth()+
    xlab(desc[[var_x]]) +
    ylab(desc[[var_y]])+
    theme(panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          panel.grid.major = element_line(color = "white", size = 0.1),
          panel.grid.minor = element_line(color = "white", size = 0.05),
          text = element_text(color = "white"))
  print(suicideVsCardeaths)
}

for( i in v161_race_vars){
  var_x <- v161_race_vars[i]
  
  FPMHDvsSuicidebyrace <- ggplot(data_analytic, aes_string( y = paste0("log(",var,")"), x = "v145_rawvalue"))+
    geom_point(color="green", alpha =0.5)+
    geom_smooth()+
    xlab(desc[["v145_rawvalue"]]) +
    ylab(desc[[var]])
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "white", size = 0.1),
        panel.grid.minor = element_line(color = "white", size = 0.05),
        text = element_text(color = "white"))
  print(FPMHDvsSuicidebyrace)
}