plot_scalegram <- function(df, stat, transparancy){
  cols <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique(df$Variable)))
  ggplot(data = df, aes(x = scale, y = y_scale)) +
    geom_line(aes(group = interaction(Variable),
                  colour = factor(Variable)), size = 0.5, alpha = transparancy) +
    geom_point(aes(group = interaction(Variable),
                   colour = factor(Variable)), alpha = transparancy) +
    scale_y_continuous(stat) +
    scale_x_log10("Aggregation scale [-]",
                  labels = trans_format("log10", math_format(10 ^ .x))) +
    scale_colour_manual("", values = cols) +
    annotation_logticks(sides = "b") +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

