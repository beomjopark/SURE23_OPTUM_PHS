round_cor_matrix <-round(cor(corr_matrix), 2)
ggcorrplot(round_cor_matrix,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)+
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white")
  )

