plot_scalegram <- function(df, log_x = T, log_y = T){
  gp <- ggplot(data = df, aes_string(x = "scale", y = colnames(df)[2])) +
    geom_line(size = 0.5) +
    geom_point() +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
  if(log_x == T){
    gpp <- gp + scale_x_log10("Aggregation scale [-]",
                              labels = trans_format("log10", math_format(10 ^ .x)),
                              breaks = trans_breaks("log10",
                                                            n = abs(round(log10(min(df[, 2], na.rm = T)))) + 1,
                                                            function(x) 10 ^ x)) +
      annotation_logticks(sides = "b")
  } else {
    gpp = gp + scale_x_continuous("Aggregation scale [-]")
  }
  if(log_y == T){
    gppp = gpp + scale_y_log10(colnames(df)[2], labels = trans_format("log10", math_format(10^.x)),
                               breaks = trans_breaks("log10",
                                                             n = abs(round(log10(min(df[, 2], na.rm = T)))) + 1,
                                                             function(x) 10 ^ x)) +
      annotation_logticks(sides = "b")
  } else {
    gppp <- gpp + scale_y_continuous(colnames(df)[2])
  }
  print(gppp)
}

