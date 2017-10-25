#' Main scalegram function
#'
#' @param x A vector, time series or a matrix.
#' @param MODE the statistic used.
#' @return A list with the scalegram of \code{x} for statistic \code{MODE} and the corresponding plot [ggplot object].
#' @examples
#' scalegram(dataset, "s1")
#' scalegram(dataset[,1], "s2")
#' @export

scalegram_parallel = function(x, MODE, STD, threshold, cores_used = detectCores() - 1, ...){
  cl = makeCluster(cores_used)
  clusterExport(cl = cl, varlist=c("x", "MODE", "STD", "threshold", "scalegram_main"), envir = environment())
  out  = parApply(cl, as.matrix(x), 2, MODE=MODE, STD=TRUE, threshold = 30, scalegram_main)
  return(out)
}
