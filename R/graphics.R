plot_scalegram <- function(df){
  if (ncol(df) == 2){ # i.e., working with only one variable
    ggplot(data = df, aes_string(x = colnames(df)[1], y = colnames(df)[2])) +
      geom_line(size = 0.5) +
      geom_point() +
      scale_y_continuous(colnames(df)[2]) +
      scale_x_log10("Aggregation scale [-]",
                    labels = trans_format("log10", math_format(10 ^ .x))) +
      annotation_logticks(sides = "b") +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())
  } else { # i.e., working with many variables
    cols <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique(df$variable)))
    if(length(unique(df$variable)) > 10){
      transp = 1 / log(length(unique(df$variable)))
    } else {
      transp = 1
    }
    ggplot(data = df, aes_string(x = colnames(df)[1], y = colnames(df)[3])) +
      geom_line(aes(group = interaction(variable),
                    colour = factor(variable)), size = 0.5, alpha = transp) +
      geom_point(aes(group = interaction(variable),
                     colour = factor(variable)), alpha = transp) +
      scale_y_continuous(colnames(df)[3]) +
      scale_x_log10("Aggregation scale [-]",
                    labels = trans_format("log10", math_format(10 ^ .x))) +
      scale_colour_manual("", values = cols) +
      annotation_logticks(sides = "b") +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())
  }
}
