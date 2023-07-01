#PHS vs Income Ineqality by  race 
#plots for y axis by race
#gather all y axis colnames - i.e. preventable hospital stays by race 
#our x axis will be the same, income inequality v044_rawvalue
plot_var_names_list <- data_analytic_interim %>% select(starts_with('v005_race')) %>% colnames
data_long_filtered <- data_long %>% filter(value <= 15000)

# Loop over the list of variable names
for (y_var in plot_var_names_list) {
  # Generate the plot
  p <- ggplot(data_analytic_interim, aes_string(x = "v044_rawvalue", y = y_var))+
    geom_point(alpha = 0.3, color='green') +
    geom_smooth() +
    labs(x = descriptions[["v044_rawvalue"]], y = descriptions[[y_var]]) +
    theme(
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      axis.title = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      legend.background = element_rect(fill = "black"),
      legend.text = element_text(color = "white")
    )
  
  # Print the plot
  print(p)
}
