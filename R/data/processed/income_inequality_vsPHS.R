##tried to use descriptions, almost but not quite 
plot_var_names = setNames(c("v044_rawvalue","v005_rawvalue"),
                          c("x", "y"))
ggplot(data_analytic_interim, aes_string(x = plot_var_names["x"] , y = plot_var_names["y"])) +
  geom_point( alpha = 0.3) +
  labs(x = descriptions[[plot_var_names["x"]]], y = descriptions[[plot_var_names["y"]]]) +
  theme_bw()


