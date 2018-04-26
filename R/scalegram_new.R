require(snow)
require(moments)
require(Lmoments)

agg <- function(x, agg_scale, agg_fun = mean) {
  aa <- tapply(x, (seq_along(x) - 1) %/% agg_scale, agg_fun, na.rm = T)
  return(aa[1:(length(aa) - 1)])
}

scalegram_fun <- function(x, thres = 30, scale_fun = var){  #x is vector
  ts_length <- length(x)
  x_df <- as.data.frame(x)
  scales <- 1:(ts_length / thres)
  out <- as.numeric(unlist(
    lapply(mapply(agg, x_df, scales), scale_fun, na.rm = T)))
  return(out)
}

scalegram_fun_par <- function(x, thres = 30, scale_fun = var){  #x is vector
  ts_length <- length(x)
  x_df <- as.data.frame(x)
  scales <- 1:(ts_length / thres)
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cl <- makeCluster(getOption("cl.cores", no_cores))
  out <- as.numeric(unlist(
    parLapply(cl, clusterMap(cl, agg, x_df, scales), scale_fun, na.rm = T)))
  stopCluster(cl)
  return(out)
}
