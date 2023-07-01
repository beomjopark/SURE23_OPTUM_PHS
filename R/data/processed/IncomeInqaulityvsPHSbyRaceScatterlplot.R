#scatter plot
# Reshape the data to long format
data_long <- data_analytic_interim %>%
  pivot_longer(cols = starts_with('v005_race'), names_to = "race", values_to = "value")

# Generate the plot
p <- ggplot(data_long, aes(x = v044_rawvalue, y = value, color = race)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(x = descriptions[["v044_rawvalue"]], y = "Preventable Hospital Stays") +
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