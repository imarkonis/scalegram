#' Rescales variance
#'
#' @param x A vector, time series or a matrix.
#' @param MODE the statistic used.
#' @return A list with the scalegram of \code{x} for statistic \code{MODE} and the corresponding plot [ggplot object].
#' @examples
#' scalegram(dataset, "s1")
#' scalegram(dataset[,1], "s2")
#' @export

rescale_variance = function(emp_scalegram_coarse, emp_scalegram_fine, scale_ratio){
  rescale_factor = emp_scalegram_fine[scale == scale_ratio]$y_scale
  dummy = emp_scalegram_coarse
  dummy$scale = dummy$scale * scale_ratio
  dummy$y_scale = t(t(dummy$y_scale) * rescale_factor)
  return(dummy)
}

