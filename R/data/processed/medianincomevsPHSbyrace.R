# Generate the plot
#data_long_filtered <- data_long %>% filter(value >= 15000)

p <- ggplot(data_long, aes(x = v044_rawvalue, y = value, color = race)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  xlab(paste(descriptions[["v063_rawvalue"]], "\n Income where half of households in a county earn more and half of households earn less.")) +
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