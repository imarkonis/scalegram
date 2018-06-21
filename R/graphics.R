scalegram_plot <- function(x, log_x = T, log_y = T, wn = F){
  colnames(x) <- c("scale", "Value")
  df <- as.data.frame(x)
  gp <- ggplot(data = df, aes_string(x = "scale", y = "Value")) +
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
    gpp <- gp + scale_x_continuous("Aggregation scale [-]")
  }
  if(log_y == T){
    gppp <- gpp + scale_y_log10("Value", labels = trans_format("log10", math_format(10^.x)),
                                breaks = trans_breaks("log10",
                                                      n = abs(round(log10(min(df[, 2], na.rm = T)))) + 1,
                                                      function(x) 10 ^ x)) +
      annotation_logticks(sides = "b")
  } else {
    gppp <- gpp + scale_y_continuous("Value")
  }
  if(wn == T){
    gppp <- gppp + geom_abline(slope = -1, size = 1, col = "dark red")
  }
  print(gppp)
}

scalegram_multiplot <- function(df, log_x = T, log_y = T){
  colnames(df) <- c("scale", "value", "variable")
  df <- as.data.frame(df)
  no_var <- length(unique(df$variable))
  cols <- colorRampPalette(c("#4575b4", "#abd9e9"), space = "rgb")(no_var)
  transp = 1 / log(no_var)

  if(no_var <= 10){
    transp = 1
    cols <- colorRampPalette(c("#4575b4", "#78c679", "#f46d43", "#74add1", "#807dba",
                               "#fee090", "#d9f0a3", "#d73027", "#abd9e9", "#fdae61"), space = "rgb")
  }

  gp = ggplot(data = df, aes_string(x = df[ ,1], y = df[ ,2])) +
    geom_line(aes(group = interaction(variable),
                  colour = factor(variable)), size = 0.5, alpha = transp) +
    geom_point(aes(group = interaction(variable),
                   colour = factor(variable)), alpha = transp) +
    scale_colour_manual("", values = cols) +
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
    gpp <- gp + scale_x_continuous("Aggregation scale [-]")
  }
  if(log_y == T){
    gppp <- gpp + scale_y_log10("Value", labels = trans_format("log10", math_format(10^.x)),
                                breaks = trans_breaks("log10",
                                                      n = abs(round(log10(min(df[, 2], na.rm = T)))) + 1,
                                                      function(x) 10 ^ x)) +
      annotation_logticks(sides = "b")
  } else {
    gppp <- gpp + scale_y_continuous("Value")
  }
  print(gppp)
}
