scalegram_main <- function(x, stat, std, threshold){
  library(moments) # load libraries inside for the 'parallel' package
  library(Lmoments)
  '%!in%' <- function(x, y)!('%in%'(x, y)) # keep function inside for the 'parallel' package
  if (stat %!in% c("mean", "sd", "var", "skew", "kurt", "cv",  # check if correct stat is provided
                   "l2", "t2", "t3", "t4"))
    stop("Error: Invalid stat. Select one of mean, sd, var,
         skew, kurt, cv, l2, t2, t3, t4.")
  nna <- sum(!is.na(x)) # actual length without accounting for missing values
  max_agg_scale <- round(nna / threshold, 0) # aggregation scale up to 30% of the sample size does not count NAs
  if (max_agg_scale != 0 & nna > 2 * threshold){ # check for adequate time series length
    scale_df <- data.frame(scale = 1:max_agg_scale, y_scale = NA)
    if (std == TRUE){
      x <- scale(x, center = TRUE, scale = TRUE)
    }
    # classic moments ------------------------------------------------------------
    if (stat == "sd"){
      scale_df[1, "y_scale"] <- sd(x, na.rm = T)
    } else if (stat == "var"){
      scale_df[1, "y_scale"] <- var(x, na.rm = T)
    } else if (stat == "skew"){
      scale_df[1, "y_scale"] <- skewness(x, na.rm = T)
    } else if (stat == "kurt"){
      scale_df[1, "y_scale"] <- kurtosis(x, na.rm = T)
    # L-moments -----------------------------------------------------------------
    } else if (stat == "l2"){ # stat: L2, L-scale
      scale_df[1, "y_scale"] <- Lmoments(x, rmax = 2, na.rm = T)[, "L2"]
    } else if (stat == "t2"){ # stat: L-moment ratio L2/L1
      scale_df[1, "y_scale"] <- Lmoments(x, rmax = 2, na.rm = T)[, "L2"] /
        Lmoments(x, rmax = 2, na.rm = T)[, "L1"]
    } else if (stat == "t3"){ # stat: L-moment ratio L3/L2
      scale_df[1, "y_scale"] <- Lcoefs(x, rmax = 4, na.rm = T)[, "tau3"]
    } else if (stat == "t4"){ # stat: L-moment ratio L4/L3
      scale_df[1, "y_scale"] <- Lcoefs(x, rmax = 4, na.rm = T)[, "tau4"]
    }
    for (i in 2:max_agg_scale){ # loop around aggregation scale
      x_agg <- as.numeric(tapply(x, (seq_along(x) - 1) %/% i, mean, na.rm = T))
      if (sum(!is.na(x_agg)) >= threshold){ # have at least 30 values for the y_scale estimation
          # classic moments -------------------------------------------------------
          if (stat == "sd"){
            scale_df[i, "y_scale"] <- sd(x_agg, na.rm = T)
          } else if (stat == "var"){
            scale_df[i, "y_scale"] <- var(x_agg, na.rm = T)
          } else if (stat == "skew"){
            scale_df[i, "y_scale"] <- skewness(x_agg, na.rm = T)
          } else if (stat == "kurt"){
            scale_df[i, "y_scale"] <- kurtosis(x_agg, na.rm = T)
            # L-moments ------------------------------------------------------------
          } else if (stat == "L2"){ # stat: L2, L-scale
            scale_df[i, "y_scale"] <- Lmoments(x_agg, rmax = 2, na.rm = T)[, "L2"]
          } else if (stat == "t2"){ # stat: L-moment ratio L2/L1
            scale_df[i, "y_scale"] <-  Lmoments(x_agg, rmax = 2, na.rm = T)[, "L2"] /
              Lmoments(x_agg, rmax = 2, na.rm = T)[, "L1"]
          } else if (stat == "t3"){ # stat: L-moment ratio L3/L2
            scale_df[i, "y_scale"] <- Lcoefs(x_agg,rmax = 4, na.rm = T)[, "tau3"]
          } else if (stat == "t4"){ # stat: L-moment ratio L4/L3
            scale_df[i, "y_scale"] <- Lcoefs(x_agg, rmax = 4, na.rm = T)[, "tau4"]
          }
        }
      } # loop around aggregation scale
    } else { # i.e., too short time series
      return("Error: Time series length too short!")
    }
    return(scale_df[complete.cases(scale_df), ])
}
