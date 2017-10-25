#' Main scalegram function called by scalegram.
#'
#' @param x A vector, time series or a matrix.
#' @param stat the statistic used.
#' @return A list with the scalegram of \code{x} for statistic \code{stat} and the corresponding plot [ggplot object].
#' @examples
#' scalegram(dataset, "s1")
#' scalegram(dataset[,1], "s2")
#' @export

scalegram_main <- function(x, stat, std, threshold){ #std: standardize the scalegram, threshold: sample size at last scale
  library(moments) # load libraries inside for the 'parallel' package
  library(Lmoments)
  '%!in%' <- function(x,y)!('%in%'(x,y)) # keep function inside for the 'parallel' package
  # check if correct stat is provided
  if (stat %!in% c("mu", "s1", "s2", "s3", "s4", "CV",
                   "L2", "t2", "t3", "t4"))
    stop("Error: Invalid stat. Select one of mu, s1, s2, s3, s4, CV, L2, t2, t3, t4.")

  nna <- sum(!is.na(x)) # actual length without accounting for missing values
  max_agg_scale <- round(nna/threshold, 0) # aggregrion scale up to 30% of the sample size does not count NAs

  if (max_agg_scale != 0 & nna > 2 * threshold){ # check for adequate time series length
    scale_df <- data.frame(scale = 1:max_agg_scale, y_scale = NA) # Create a data.frame to store all the y_scale

    if (std == TRUE){ # standardize data ,i.e., zero mean unit variance at the original time scale
      x <- scale(x, center = TRUE, scale = TRUE)
    }

    # classic moments ------------------------------------------------------------
    if (stat == "mu"){ # stat: mean
      scale_df[1,"y_scale"] <- mean(x, na.rm=T)
    } else if (stat == "s1"){ # stat: standard deviation
      scale_df[1,"y_scale"] <- sd(x, na.rm=T)
    } else if (stat == "s2"){ # stat: variance
      scale_df[1,"y_scale"] <- var(x, na.rm=T)
    } else if (stat == "s3"){ # stat: skewness
      scale_df[1,"y_scale"] <- skewness(x, na.rm=T)
    } else if (stat == "s4"){ # stat: kurtosis
      scale_df[1,"y_scale"] <- kurtosis(x, na.rm=T)
    } else if (stat == "CV"){ # stat: coefficient of variation
      scale_df[1,"y_scale"] <- sd(x, na.rm=T)/mean(x, na.rm=T)
    # L-moments -----------------------------------------------------------------
    } else if (stat == "L2"){ # stat: L2, L-scale
      scale_df[1,"y_scale"] <- Lmoments(x, rmax=2, na.rm=T)[, "L2"]
    } else if (stat == "t2"){ # stat: L-moment ratio L2/L1
      scale_df[1,"y_scale"] <- Lmoments(x, rmax=2, na.rm=T)[, "L2"] / Lmoments(x, rmax=2, na.rm=T)[, "L1"]
    } else if (stat == "t3"){ # stat: L-moment ratio L3/L2
      scale_df[1,"y_scale"] <- Lcoefs(x, rmax=4, na.rm=T)[, "tau3"]
    } else if (stat == "t4"){ # stat: L-moment ratio L4/L3
      scale_df[1, "y_scale"] <- Lcoefs(x, rmax=4, na.rm=T)[, "tau4"]
    }

    for (i in 2:max_agg_scale){ # loop around aggregation scale
      x_agg = as.numeric(tapply(x, (seq_along(x) - 1) %/% i, mean, na.rm = T))

      if (sum(!is.na(x_agg))>=threshold){ # have at least 30 values for the y_scale estimation
          # classic moments -------------------------------------------------------
          if (stat == "mu"){ # stat: mean
            scale_df[i,"y_scale"] <- mean(x_agg, na.rm=T)

          } else if (stat == "s1"){ # stat: standard deviation
            scale_df[i,"y_scale"] <- sd(x_agg, na.rm=T)

          } else if (stat == "s2"){ # stat: variance
            scale_df[i,"y_scale"] <- var(x_agg, na.rm=T)

          } else if (stat == "s3"){ # stat: skewness
            scale_df[i,"y_scale"] <- skewness(x_agg, na.rm=T)

          } else if (stat == "s4"){ # stat: kurtosis
            scale_df[i,"y_scale"] <- kurtosis(x_agg, na.rm=T)

          } else if (stat == "CV"){ # stat: coefficient of variation
            scale_df[i,"y_scale"] <- sd(x_agg, na.rm=T)/mean(x_agg, na.rm=T)

            # L-moments ------------------------------------------------------------
          } else if (stat == "L2"){ # stat: L2, L-scale
            scale_df[i,"y_scale"] <- Lmoments(x_agg, rmax=2,na.rm=T)[,"L2"]

          } else if (stat == "t2"){ # stat: L-moment ratio L2/L1
            scale_df[i,"y_scale"] <-  Lmoments(x_agg, rmax=2,na.rm=T)[,"L2"]/Lmoments(x_agg, rmax=2,na.rm=T)[,"L1"]

          } else if (stat == "t3"){ # stat: L-moment ratio L3/L2
            scale_df[i,"y_scale"] <- Lcoefs(x_agg,rmax=4,na.rm=T)[,"tau3"]

          } else if (stat == "t4"){ # stat: L-moment ratio L4/L3
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


