corr_data <- data_analytic %>% 
  dplyr::select(
    #income inequality  
    v044_rawvalue,
    
    #high school completion
    v023_rawvalue, 
    
    #unemployment
    v168_rawvalue,
    
    #preventable stays
    v005_rawvalue,v005_race_asian, v005_race_aian, v005_race_hispanic, v005_race_white, v005_race_black,
  ) 
colnames(corr_data) = descriptions %>% select(colnames(corr_data))
corr_matrix <- cor(corr_data, use = "complete.obs")
ggcorrplot(corr_matrix)+
  labs(x = "v044_rawvalue\nFootnote for x-axis variable",
       y = "v063_rawvalue\nFootnote for y-axis variable") +
  theme(axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 2),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white")
  )