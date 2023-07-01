# Generate the plot
data_long_filtered <- data_long %>% filter(value <= 15000)

p <- ggplot(data_long_filtered, aes(x = v044_rawvalue, y = value, color = race)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  xlab(paste(descriptions[["v044_rawvalue"]], "\nRatio of household income at the 80th percentile to income at the 20th percentile.")) +
  ylab("Preventable Hospital Stays") +
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