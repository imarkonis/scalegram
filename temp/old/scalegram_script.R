scalegram_main <- function(x, stat, std, threshold, fast){
  '%!in%' <- function(x, y)!('%in%'(x, y)) # keep function inside for the 'parallel' package
  if (stat %!in% c("mean", "sd", "var", "skew", "kurt", "cv", "l2", "t2", "t3", "t4"))
    stop("Error: Invalid stat. Select one of mean, sd, var, skew, kurt, cv, l2, t2, t3, t4.")

  out <- vector()
  nna <- sum(!is.na(x)) # actual length without accounting for missing values
  max_agg_scale <- round(nna / threshold, 0) # aggregation scale up to 30% of the sample size does not count NAs
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
    if (std == T){
      x <- scale(x, center = T, scale = T)
    }
    out <- foreach (i = timescales) %dopar%  {# loop around aggregation scale
      x_agg <- as.numeric(tapply(x, (seq_along(x) - 1) %/% i, mean, na.rm = T)) # estimate aggregated values for scale i
      x_agg <- x_agg[-length(x_agg)]
      if (sum(!is.na(x_agg)) >= threshold){ # have at least 30 values for the y_scale estimation
        # classic moments -------------------------------------------------------
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
    } # loop around aggregation scale
  } else {
    return("Error: Time series length too short!")
  }
  stopCluster(cluster)
  out <- data.frame(scale = timescales[1:(length(timescales)-1)], value = unlist(out))
  return(out)
}
