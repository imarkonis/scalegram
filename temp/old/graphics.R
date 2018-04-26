plot_scalegram <- function(df, logscale_x=T, logscale_y=T){
  if (length(unique(df$variable)) == 1){ # i.e., working with only one variable
    gp = ggplot(data = df, aes_string(x = "scale", y = "value")) +
      geom_line(size = 0.5) +
      geom_point() +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())
  } else { # i.e., working with many variables
    cols <- colorRampPalette(c("#4575b4", "#abd9e9"), space = "rgb")(length(unique(df$variable)))
    if(length(unique(df$variable)) == 2){
    transp = 0.8 
    }else{
    transp = 1 / log(length(unique(df$variable)))
    }
    gp = ggplot(data = df, aes_string(x = "scale", y = "value")) +
      geom_line(aes(group = interaction(variable),
                    colour = factor(variable)), size = 0.5, alpha = transp) +
      geom_point(aes(group = interaction(variable),
                     colour = factor(variable)), alpha = transp) +
      scale_colour_manual("", values = cols) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())
  }
  
  if(logscale_x == T){
  gpp = gp + scale_x_log10("Aggregation scale [-]", labels = trans_format("log10", math_format(10 ^ .x))) + annotation_logticks(sides = "b")
  } else {
  gpp = gp + scale_x_continuous("Aggregation scale [-]")
  }
  if(logscale_y == T){
  gppp = gpp + scale_y_log10(colnames(df)[2], labels = trans_format("log10", math_format(10 ^ .x)), breaks=c(round(min(df[,"value"], na.rm=T), 5), 1)) + annotation_logticks(sides = "b")
  } else {
  gppp = gpp + scale_y_continuous(colnames(df)[2]) 
  }
  
  print(gppp)

}


