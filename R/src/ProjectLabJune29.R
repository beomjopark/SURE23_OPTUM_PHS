# PLOT 

# Read data
data_analytic_interim = readRDS("./data/interim/analytic2023_c1.rds")
descriptions = readRDS("./data/processed/colDesc_analysis2023.rds")

plot_var_names = setNames(c("v044_rawvalue","v005_rawvalue"),
                          c("x", "y"))

# Race-adjsted PHS vs Income Ineq
ggplot(data_analytic_interim, aes_string(x = plot_var_names["x"] , y = plot_var_names["y"])) +
  geom_point( alpha = 0.3) +
  geom_smooth(method="loess", col="red") +
  labs(x = descriptions[[plot_var_names["x"]]], y = descriptions[[plot_var_names["y"]]]) +
  theme_minimal()

with(data_analytic_interim %>% select(all_of(plot_var_names)),
     cor(x,y, use="complete.obs"))
#[1] 0.3126361

####
data_long_filtered = data_long %>% filter(value > 10000) %>% 
  select(-(ends_with("_denominator") | ends_with("_numerator") |
             ends_with("_cilow") | ends_with("_cihigh") |
             ends_with("_flag") |
             ends_with("_race_aian")| ends_with("_race_black") |
             ends_with("_race_white") | ends_with("_race_hispanic") |
             ends_with("_race_asian") |
             ends_with("_other_data_1") | ends_with("_other_data_2") | ends_with("_other_data_3")
             ) ) %>%
  select(- all_of(c("statecode", "countycode"))) 
