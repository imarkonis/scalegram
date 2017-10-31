scalegram_parallel = function(x, stat, std, threshold, cores_used = detectCores() - 1, ...){
  cl = makeCluster(cores_used)
  clusterExport(cl = cl, varlist=c("x", "stat", "std", "threshold", "scalegram_main"), envir = environment())
  out = parApply(cl, as.matrix(x), 2, stat=stat, std=std, threshold = threshold, scalegram_main)
  return(out)
}
