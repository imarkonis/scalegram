#' Rescales variance
#'
#' @param scalegram_coarse A vector, time series or a matrix.
#' @param scalegram_fine the statistic used.
#' @param scale_ratio the ratio between coarser and finer scale.
#' @param stat the statistic used.
#' @return A list with the scalegram of \code{x} for statistic \code{MODE} and the corresponding plot [ggplot object].
#'
#' @examples
#' site_a = owda[owda$Lat == 46.25 & owda$Lon == 16.5,]$scPDSI
#' site_b = owda[owda$Lat == 46.25 & owda$Lon == 16.5 & owda$Time %in% seq(992, 2012, 2) ,]$scPDSI
#' sg_a = scalegram(site_a)
#' sg_b = scalegram(site_b)
#' sg_a$scalegram_plot +
#' geom_line(data=sg_b$scalegram_df, col = "red") +
#' geom_point(data=sg_b$scalegram_df, col = "red")
#'
#'  \item{"mean" for mean,}
#'  \item{"cv" for coefficient of variance,}
#' @export

rescale_scalegram <- function(scalegram_coarse, scalegram_fine, scale_ratio, stat){
  rescale_factor = scalegram_fine[scalegram_fine$scale == scale_ratio, stat]
  dummy = scalegram_coarse
  dummy$scale = dummy$scale * scale_ratio
  dummy[,stat] = t(t(dummy[,stat]) * rescale_factor)
  return(dummy)
}


