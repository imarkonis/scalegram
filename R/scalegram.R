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
  # Parallel computing
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
    } # parallel loop around aggregation scale
  } else {
    return("Error: Time series length too short!")
  }
  stopCluster(cluster)
  out <- out[-length(out)]
  out <- matrix(c(timescales[1:length(out)], out), ncol = 2)
  colnames(out) = c("scale", stat)

  if (plot == T){
    plot_sc <- scalegram_plot(out, ...)
    return(list(sg_df   = out,
                sg_plot = plot_sc))
  }
  else {
    return(out)
  }
}

scalegram_space <- function(x, stat = "var", std = T, plot = T, threshold = 30, ...){
  '%!in%' <- function(x, y)!('%in%'(x, y)) # keep function inside for the 'parallel' package
  if (stat %!in% c("mean", "sd", "var", "skew", "kurt", "cv", "l2", "t2", "t3", "t4"))
    stop("Error: Invalid stat. Select one of mean, sd, var, skew, kurt, cv, l2, t2, t3, t4.")

  ncells <- x@ncols * x@nrows
  max_agg_scale <- round(sqrt(ncells / threshold), 0) # aggregation scale up to sample size of 30 values does not count NAs
  x_agg <- list()

  # Parallel computing
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster = makeCluster(no_cores, type = "SOCK")
  registerDoSNOW(cluster)

  if (max_agg_scale != 0 & ncells > 2 * threshold){ # check for adequate time series length
    no_layer <- nlayers(x)

    out <- foreach (j = 1:no_layer, .packages = "raster") %dopar%  {# parallel loop around aggregation scale
      if(no_layer > 1){
        x_layer <- raster(x, layer = j)
      }
      else{
        x_layer <- x
      }
      if (std == T){  # standardize
        x_layer[,] <- scale(x_layer[,], center = T, scale = T)
      }
      for (i in 1:max_agg_scale){
        x_agg[[i]] <- aggregate(x_layer, na.rm = T, fact = i)
      }
      if (stat == "sd"){sapply(sapply(x_agg, getValues), sd, na.rm = T)}
      else if (stat == "var"){sapply(sapply(x_agg, getValues), var, na.rm = T)}
      else if (stat == "skew"){sapply(sapply(x_agg, getValues), skewness, na.rm = T)}
      else if (stat == "kurt"){sapply(sapply(x_agg, getValues), mean, na.rm = T)}
    } # parallel loop around aggregation scale

    out <- data.table(melt(out))
    out$scale <- rep(1:nrow(out[L1 == 1]), max(out$L1))
    colnames(out)[2] = "variable"
    out <- out[, c(3, 1, 2)]
  } else {
    return("Error: Time series length too short!")
  }
  stopCluster(cluster)

  if (plot == T){
    if(ncol(out) == 2){
      plot_sc <- scalegram_plot(out, ...)
    }
    else{
      plot_sc <- scalegram_multiplot(out, ...)
    }
    return(list(sg_df   = out,
                sg_plot = plot_sc))
  }
  else {
    return(out)
  }
}

dt_to_brick <- function(dt, var_name) {
  arr_from_dt <- acast(dt, lat ~ lon ~ time, value.var = var_name, fun.aggregate = mean)
  out <- brick(arr_from_dt, ymn = min(as.numeric(rownames(arr_from_dt))),
               ymx = max(as.numeric(rownames(arr_from_dt))), xmn = min(as.numeric(colnames(arr_from_dt))),
               xmx = max(as.numeric(colnames(arr_from_dt))))
  out <- flip(out, direction = "2")
  return(out)
}

scalegram_rescale <- function(scalegram_coarse, scalegram_fine, scale_ratio){
  rescale_factor = scalegram_fine[scale == scale_ratio]$value
  dummy = scalegram_coarse
  dummy$scale = dummy$scale * scale_ratio
  dummy$value = t(t(dummy$value) * rescale_factor)
  return(dummy)
}
