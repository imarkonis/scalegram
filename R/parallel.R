#' Function for parallel computing used when input data are in matrix format.
#'
#' @param x A vector, time series or a matrix.
#' @param stat the statistic used.
#' @return A list with the scalegram of \code{x} for statistic \code{stat} and the corresponding plot [ggplot object].
#' @examples
#' scalegram(dataset, "s1")
#' scalegram(dataset[,1], "s2")
#' @export

scalegram_parallel = function(x, stat, std, threshold, cores_used = detectCores() - 1, ...){
  cl = makeCluster(cores_used)
  clusterExport(cl = cl, varlist=c("x", "stat", "std", "threshold", "scalegram_main"), envir = environment())
  out  = parApply(cl, as.matrix(x), 2, stat=stat, std=TRUE, threshold = 30, scalegram_main)
  return(out)
}
