## Empirical scalegrams

scalegram_main <- function(x, MODE, STD, threshold){ #STD: standardize the scalegram, threshold: sample size at last scale
  library(moments) # load libraries inside for the 'parallel' package
  library(Lmoments)
  '%!in%' <- function(x,y)!('%in%'(x,y)) # keep function inside for the 'parallel' package
  # check if correct MODE is provided
  if (MODE %!in% c("mu", "s1", "s2", "s3", "s4", "CV",
                   "L2", "t2", "t3", "t4"))
    stop("Error: Invalid MODE. Select one of mu, s1, s2, s3, s4, CV, L2, t2, t3, t4.")

  nna <- sum(!is.na(x)) # actual length without accounting for missing values
  max_agg_scale <- round(nna/threshold, 0) # aggregrion scale up to 30% of the sample size does not count NAs

  if (max_agg_scale != 0 & nna > 2 * threshold){ # check for adequate time series length
    scale_df <- data.frame(scale = 1:max_agg_scale, y_scale = NA) # Create a data.frame to store all the y_scale

    if (STD == TRUE){ # standardize data ,i.e., zero mean unit variance at the original time scale
      x <- scale(x, center = TRUE, scale = TRUE)
    }

    # classic moments ------------------------------------------------------------
    if (MODE == "mu"){ # MODE: mean
      scale_df[1,"y_scale"] <- mean(x, na.rm=T)
    } else if (MODE == "s1"){ # MODE: standard deviation
      scale_df[1,"y_scale"] <- sd(x, na.rm=T)
    } else if (MODE == "s2"){ # MODE: variance
      scale_df[1,"y_scale"] <- var(x, na.rm=T)
    } else if (MODE == "s3"){ # MODE: skewness
      scale_df[1,"y_scale"] <- skewness(x, na.rm=T)
    } else if (MODE == "s4"){ # MODE: kurtosis
      scale_df[1,"y_scale"] <- kurtosis(x, na.rm=T)
    } else if (MODE == "CV"){ # MODE: coefficient of variation
      scale_df[1,"y_scale"] <- sd(x, na.rm=T)/mean(x, na.rm=T)
    # L-moments -----------------------------------------------------------------
    } else if (MODE == "L2"){ # MODE: L2, L-scale
      scale_df[1,"y_scale"] <- Lmoments(x, rmax=2, na.rm=T)[, "L2"]
    } else if (MODE == "t2"){ # MODE: L-moment ratio L2/L1
      scale_df[1,"y_scale"] <- Lmoments(x, rmax=2, na.rm=T)[, "L2"] / Lmoments(x, rmax=2, na.rm=T)[, "L1"]
    } else if (MODE == "t3"){ # MODE: L-moment ratio L3/L2
      scale_df[1,"y_scale"] <- Lcoefs(x, rmax=4, na.rm=T)[, "tau3"]
    } else if (MODE == "t4"){ # MODE: L-moment ratio L4/L3
      scale_df[1, "y_scale"] <- Lcoefs(x, rmax=4, na.rm=T)[, "tau4"]
    }

    for (i in 2:max_agg_scale){ # loop around aggregation scale
      x_agg = as.numeric(tapply(x, (seq_along(x) - 1) %/% i, mean, na.rm = T))

      if (sum(!is.na(x_agg))>=threshold){ # have at least 30 values for the y_scale estimation
          # classic moments -------------------------------------------------------
          if (MODE == "mu"){ # MODE: mean
            scale_df[i,"y_scale"] <- mean(x_agg, na.rm=T)

          } else if (MODE == "s1"){ # MODE: standard deviation
            scale_df[i,"y_scale"] <- sd(x_agg, na.rm=T)

          } else if (MODE == "s2"){ # MODE: variance
            scale_df[i,"y_scale"] <- var(x_agg, na.rm=T)

          } else if (MODE == "s3"){ # MODE: skewness
            scale_df[i,"y_scale"] <- skewness(x_agg, na.rm=T)

          } else if (MODE == "s4"){ # MODE: kurtosis
            scale_df[i,"y_scale"] <- kurtosis(x_agg, na.rm=T)

          } else if (MODE == "CV"){ # MODE: coefficient of variation
            scale_df[i,"y_scale"] <- sd(x_agg, na.rm=T)/mean(x_agg, na.rm=T)

            # L-moments ------------------------------------------------------------
          } else if (MODE == "L2"){ # MODE: L2, L-scale
            scale_df[i,"y_scale"] <- Lmoments(x_agg, rmax=2,na.rm=T)[,"L2"]

          } else if (MODE == "t2"){ # MODE: L-moment ratio L2/L1
            scale_df[i,"y_scale"] <-  Lmoments(x_agg, rmax=2,na.rm=T)[,"L2"]/Lmoments(x_agg, rmax=2,na.rm=T)[,"L1"]

          } else if (MODE == "t3"){ # MODE: L-moment ratio L3/L2
            scale_df[i,"y_scale"] <- Lcoefs(x_agg,rmax=4,na.rm=T)[,"tau3"]

          } else if (MODE == "t4"){ # MODE: L-moment ratio L4/L3
            scale_df[i,"y_scale"] <- Lcoefs(x_agg,rmax=4,na.rm=T)[,"tau4"]
          }
        }
      } # loop around aggregation scale

    } else { # i.e., too short time series
      return("Error: Time series length too short!")
    }

    # return only complete cases
    return(scale_df[complete.cases(scale_df),])

}




scalegram_parallel = function(x, MODE, STD, threshold, cores_used = detectCores() - 1, ...){
  cl = makeCluster(cores_used)
  clusterExport(cl = cl, varlist=c("x", "MODE", "STD", "threshold", "scalegram_main"), envir = environment())
  out  = parApply(cl, as.matrix(x), 2, MODE=MODE, STD=TRUE, threshold = 30, scalegram_main)
  return(out)
}



scalegram = function(x, MODE = "s1", STD = TRUE, threshold = 30, PLOT = TRUE){

  if (!is.numeric(x)) stop("x should be numeric.")
  if (!is.vector(x) & !is.ts(x) & !is.matrix(x)) stop("x should be either vector, or time series, or matrix object.")

  if(is.ts(x)) x = as.vector(x)

  if(is.vector(x)) {
    out = scalegram_main(x, MODE, STD, threshold)
    out = out[complete.cases(out),]
    out$Variable = "variable"
  } else {
    out = scalegram_parallel(x, MODE, STD, threshold)

    # transform from 'list' to 'matrix'
    rows_diff <- max(sapply(out, function(x) max(x[[1]])))
    columns <- length(out)
    # make an empty matrix
    out_matr = matrix(NA, nrow=rows_diff, ncol=columns)
    rownames(out_matr) = 1:rows_diff
    colnames(out_matr) = 1:columns
    # fill the matrix
    for (i in 1:columns){# loop through variables
      out_matr[out[[i]]$scale,i] <- out[[i]]$y_scale
    }# loop through trees
    out = melt(out_matr)
    colnames(out) = c("scale", "Variable", "y_scale")
    out = out[complete.cases(out),]
  }

  if (PLOT == TRUE){

    if(length(unique(out$Variable))>10){
      transp = 1/log(length(unique(out$Variable)))
    } else {
      transp = 1
    }

    plot_sc = plot_scalegram(out, MODE, transparancy=transp)
    print(plot_sc)

    # return only complete cases
    return(list(scalegram_df = out,
                scalegram_plot = plot_sc))
  } else {
    return(out)
  }

}
