scalegram  <- function(x, stat = "var", std = T, threshold = 30, plot = T, fast = F, ...) {
  if (!is.numeric(x)) stop ("x should be numeric.")
  if (!is.vector(x)) stop ("x should be vector.")
  '%!in%' <- function(x, y)!('%in%'(x, y)) # keep function inside for the 'parallel' package
  if (stat %!in% c("mean", "sd", "var", "skew", "kurt", "cv", "l2", "t2", "t3", "t4"))
    stop("Error: Invalid stat. Select one of mean, sd, var, skew, kurt, cv, l2, t2, t3, t4.")

  out <- vector()
  nna <- sum(!is.na(x)) # actual length without accounting for missing values
  max_agg_scale <- round(nna / threshold, 0) # aggregation scale up to sample size of 30 values does not count NAs
  timescales <- 1:max_agg_scale
  if(fast == T){
    timescales <- c(1:9 %o% 10^(0:30))
    timescales <- timescales[timescales <= max_agg_scale]
  }
  # Parralel computing
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster = makeCluster(no_cores, type = "SOCK")
  registerDoSNOW(cluster)

  if (max_agg_scale != 0 & nna > 2 * threshold){ # check for adequate time series length
    if (std == T){  # standardize
      x <- scale(x, center = T, scale = T)
    }
    out <- foreach (i = timescales, .combine = 'c') %dopar%  {# parallel loop around aggregation scale
      x_agg <- as.numeric(tapply(x, (seq_along(x) - 1) %/% i, mean, na.rm = T)) # estimate aggregated values for scale i
      if (sum(!is.na(x_agg)) > threshold){ # have at least 30 values for the y_scale estimation
        # classic moments ------------------------------------------------------
        if (stat == "sd"){sd(x_agg, na.rm = T)}
        else if (stat == "var"){var(x_agg, na.rm = T)}
        else if (stat == "skew"){skewness(x_agg, na.rm = T)}
        else if (stat == "kurt"){kurtosis(x_agg, na.rm = T)}
        # L-moments ------------------------------------------------------------
        else if (stat == "l2"){Lmoments(x_agg, rmax = 2, na.rm = T)[, "L2"]}  # stat: L2, L-scale
        else if (stat == "t2"){out[i, "y_scale"] <- Lmoments(x_agg, rmax = 2, na.rm = T)[, "L2"] /
          Lmoments(x_agg, rmax = 2, na.rm = T)[, "L1"]}                       # stat: L-moment ratio L2/L1
        else if (stat == "t3"){Lcoefs(x_agg,rmax = 4, na.rm = T)[, "tau3"]}   # stat: L-moment ratio L3/L2
        else if (stat == "t4"){Lcoefs(x_agg, rmax = 4, na.rm = T)[, "tau4"]}  # stat: L-moment ratio L4/L3
      }
    } # paralel loop around aggregation scale
  } else {
    return("Error: Time series length too short!")
  }
  stopCluster(cluster)
  out <- out[-length(out)]
  out <- matrix(c(timescales[1:length(out)], out), ncol = 2)
  colnames(out) = c("scale", stat)

  if (plot == T){
    plot_sc <- plot_scalegram(out, ...)
    return(list(sg_df   = out,
                sg_plot = plot_sc))
  }
  else {
    return(out)
  }
}

scalegram_brick <- function(x, thres = 30){
  no_layer <- nlayers(x)
  out <- list()
  for(j in 1:no_layer){
    i <- 2
    x_layer <- x[[j]]
    x_layer[,] <- scale(x_layer[,], center = T, scale = T)
    ncells <- length(x_layer)
    x_agg <- list(x_layer)
    while(ncells > thres){
      x_agg[[i]] <- aggregate(x_layer, fact = i)
      ncells <- length(x_agg[[i]])
      i <- i + 1
    }
    out[[j]] <- sapply(sapply(x_agg, getValues), sd, na.rm = T)
  }
  out <- data.table(melt(out))
  out$scale <- rep(1:nrow(out[L1 == 1]), max(out$L1))
  colnames(out)[2] = "variable"
  return(out[, c(3, 1, 2)])
}
