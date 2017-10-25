#' Plots the scalegram.
#'
#' @param x A vector, time series or a matrix.
#' @param stat the statistic used.
#' @return A list with the scalegram of \code{x} for statistic \code{stat} and the corresponding plot [ggplot object].
#' @examples
#' scalegram(dataset, "s1")
#' scalegram(dataset[,1], "s2")
#' @export

plot_scalegram = function(df, stat, transparancy){

  cols = colorRampPalette(brewer.pal(9, "Set1"))(length(unique(df$Variable)))

   ggplot(data = df, aes(x = scale, y = y_scale))+
    geom_line(aes(group = interaction(Variable),
                  colour = factor(Variable)), size = 0.5, alpha = transparancy) +
    geom_point(aes(group = interaction(Variable),
                   colour = factor(Variable)), alpha = transparancy) +
    scale_y_continuous(stat) +
    scale_x_log10("Aggregation scale [-]",
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_colour_manual("", values = cols)+
    annotation_logticks(sides = "b") +
    theme_bw()+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
}

