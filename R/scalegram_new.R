library(snow)

agg <- function(x, agg.scale, FUN = mean) {
  aa <- tapply(x, (seq_along(x) - 1) %/% agg.scale, FUN, na.rm = T)
  return(aa[1:(length(aa) - 1)])
}

no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
cl <- makeCluster(getOption("cl.cores", no_cores))
test <- as.numeric(unlist(parLapply(cl,
                                    clusterMap(cl, agg, as.data.frame(rnorm(10000)), 1:(10000/30)), sd)))
stopCluster(cl)



