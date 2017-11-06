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
    cols <- colorRampPalette(c("#4575b4", "#78c679", "#f46d43", "#74add1", "#807dba", "#fee090", "#d9f0a3", "#d73027",
                               "#abd9e9", "#fdae61", "#fa9fb5", "#ffed6f"), space = "rgb")(length(unique(df$variable)))

    if(length(unique(df$variable)) > 20){
      transp = 1 / log(length(unique(df$variable)))
    } else {
      transp = 1
    }
    ggplot(data = df, aes_string(x = colnames(df)[1], y = colnames(df)[2])) +
      geom_line(aes(group = interaction(variable),
                    colour = factor(variable)), size = 0.5, alpha = transp) +
      geom_point(aes(group = interaction(variable),
                     colour = factor(variable)), alpha = transp) +
      scale_y_continuous(colnames(df)[2]) +
      scale_x_log10("Aggregation scale [-]",
                    labels = trans_format("log10", math_format(10 ^ .x))) +
      scale_colour_manual("", values = cols) +
      annotation_logticks(sides = "b") +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())
  }
}
