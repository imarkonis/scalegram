## Empirical scalegrams

scalegram_main <- function(x, MODE = "s1", STD = TRUE, threshold = 30){ #Right
  library(data.table) # load libraries inside for the 'parallel' package
  library(moments)
  library(Lmoments)
  '%!in%' <- function(x,y)!('%in%'(x,y)) # keep function inside for the 'parallel' package
  # check if correct MODE is provided
  if(MODE %!in% c("mu", "s1", "s2", "s3", "s4", "CV",
                  "L1", "L2", "t2", "t3", "t4")){
    return("Error: Invalid MODE. Select one of s1, s2, s3, s4, CV, L1, L2, t2, t3, t4")
  } else {
    nna <- sum(!is.na(x)) # actual length without accounting for missing values
    delta <- round(nna/threshold, 0) # aggregrion scale up to 30% of the sample size does not count NAs

    if (delta!=0 & nna > 2 * threshold){ # check for adequate time series length
      scaledf <- data.frame(scale=1:delta, y_scale=NA) # Create a data.frame to store all the y_scale

      if (STD == TRUE){ # standardize data ,i.e., zero mean unit variance at the original time scale
        samp <- data.table(scale(x, center = TRUE, scale = TRUE))
      } else {
        samp <- data.table(x)
      }

      # normal moments ------------------------------------------------------------
      if (MODE == "mu"){ # MODE: mean
        scaledf[1,"y_scale"]    <- mean(samp$V1, na.rm=T)
      } else if (MODE == "s1"){ # MODE: standard deviation
        scaledf[1,"y_scale"]    <- sd(samp$V1, na.rm=T)
      } else if (MODE == "s2"){ # MODE: variance
        scaledf[1,"y_scale"]    <- var(samp$V1, na.rm=T)
      } else if (MODE == "s3"){ # MODE: skewness
        scaledf[1,"y_scale"]    <- skewness(samp$V1, na.rm=T)
      } else if (MODE == "s4"){ # MODE: kurtosis
        scaledf[1,"y_scale"]    <- kurtosis(samp$V1, na.rm=T)
      } else if (MODE == "CV"){ # MODE: coefficient of variation
        scaledf[1,"y_scale"]    <- sd(samp$V1, na.rm=T)/mean(samp$V1, na.rm=T)
        # L-moments -----------------------------------------------------------------
      } else if (MODE == "L1"){ # MODE: L1, L-mean, L-location
        scaledf[1,"y_scale"]    <- Lmoments(samp$V1, rmax=2,na.rm=T)[,"L1"]
      } else if (MODE == "L2"){ # MODE: L2, L-scale
        scaledf[1,"y_scale"]    <- Lmoments(samp$V1, rmax=2,na.rm=T)[,"L2"]
      } else if (MODE == "t2"){ # MODE: L-moment ratio L2/L1
        scaledf[1,"y_scale"]    <- Lmoments(samp$V1, rmax=2,na.rm=T)[,"L2"]/Lmoments(samp$V1, rmax=2,na.rm=T)[,"L1"]
      } else if (MODE == "t3"){ # MODE: L-moment ratio L3/L2
        scaledf[1,"y_scale"]    <- Lcoefs(samp$V1,rmax=4,na.rm=T)[,"tau3"]
      } else if (MODE == "t4"){ # MODE: L-moment ratio L4/L3
        scaledf[1,"y_scale"]    <- Lcoefs(samp$V1,rmax=4,na.rm=T)[,"tau4"]
      }

      for (i in 2:delta){ # loop around aggregation scale
        dummy <- rep(1:floor(nna/i), each=i)
        ttest <- samp[1:length(dummy), list(AVERAGE=mean(V1)), by=list(dummy)]$AVERAGE
        if (sum(!is.na(ttest))>=threshold){ # have at least 30 values for the y_scale estimation     # normal moments -------------------------------------------------------
          if (MODE == "mu"){ # MODE: mean
            dummyscale <- mean(ttest, na.rm=T)
            scaledf[i,"y_scale"] <- dummyscale
          } else if (MODE == "s1"){ # MODE: standard deviation
            dummyscale <- sd(ttest, na.rm=T)
            scaledf[i,"y_scale"] <- dummyscale
          } else if (MODE == "s2"){ # MODE: variance
            dummyscale <- var(ttest, na.rm=T)
            scaledf[i,"y_scale"] <- dummyscale
          } else if (MODE == "s3"){ # MODE: skewness
            dummyscale <- skewness(ttest, na.rm=T)
            scaledf[i,"y_scale"] <- dummyscale
          } else if (MODE == "s4"){ # MODE: kurtosis
            dummyscale <- kurtosis(ttest, na.rm=T)
            scaledf[i,"y_scale"] <- dummyscale
          } else if (MODE == "CV"){ # MODE: coefficient of variation
            dummyscale <- sd(ttest, na.rm=T)/mean(ttest, na.rm=T)
            scaledf[i,"y_scale"] <- dummyscale
            # L-moments ------------------------------------------------------------
          } else if (MODE == "L1"){ # MODE: L1, L-mean, L-location
            dummyscale <- Lmoments(ttest, rmax=2,na.rm=T)[,"L1"]
            scaledf[i,"y_scale"] <- dummyscale
          } else if (MODE == "L2"){ # MODE: L2, L-scale
            dummyscale <- Lmoments(ttest, rmax=2,na.rm=T)[,"L2"]
            scaledf[i,"y_scale"] <- dummyscale
          } else if (MODE == "t2"){ # MODE: L-moment ratio L2/L1
            dummyscale <- Lmoments(ttest, rmax=2,na.rm=T)[,"L2"]/Lmoments(ttest, rmax=2,na.rm=T)[,"L1"]
            scaledf[i,"y_scale"] <- dummyscale
          } else if (MODE == "t3"){ # MODE: L-moment ratio L3/L2
            dummyscale <- Lcoefs(ttest,rmax=4,na.rm=T)[,"tau3"]
            scaledf[i,"y_scale"] <- dummyscale
          } else if (MODE == "t4"){ # MODE: L-moment ratio L4/L3
            dummyscale <- Lcoefs(ttest,rmax=4,na.rm=T)[,"tau4"]
            scaledf[i,"y_scale"] <- dummyscale
          }
        }
      } # loop around aggregation scale

    }else{ # i.e., too short time series, return NAs
      return("Error: Time series length too short!")
    }
    return(scaledf[complete.cases(scaledf),])
  }
}


## Plot scalegram ==============================================================
##==============================================================================
plot_scalegram = function(X, ...){

  '%!in%' <- function(x,y)!('%in%'(x,y)) # keep function inside for the 'parallel' package

  # check if correct MODE is provided
  if(MODE %!in% c("mu", "s1", "s2", "s3", "s4", "CV",
                  "L1", "L2", "t2", "t3", "t4")){
    return("Error: Invalid MODE. Select one of s1, s2, s3, s4, CV, L1, L2, t2, t3, t4")
  } else {

    if(is.ts(X)) {X = scalegram_main(as.vector(X), MODE)
    } else if(is.vector(X)) {X = scalegram_main(X, MODE)
    }
    if("variable" %in% colnames(X)){

      if(length(unique(X$variable))>10){
        aa = 0.2
      } else {
        aa=1
      }
      ggplot(data=X, aes(x=scale, y=y_scale))+
        geom_line(aes(group=interaction(variable),
                      colour = variable), show.legend = FALSE, size = 0.5, alpha = aa) +
        geom_point(aes(group=interaction(variable),
                       colour=variable), show.legend = FALSE, alpha = aa) +
        geom_tile(aes(fill=variable))+

        scale_y_continuous(MODE) +
        scale_x_log10("Aggregation scale [-]",
                      labels = trans_format("log10", math_format(10^.x))) +
        annotation_logticks(sides = "b") +
        theme_bw()+
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank())
      #      coord_fixed()
    } else {
      ggplot(data=X, aes(x=scale, y=y_scale))+
        geom_line(show.legend = FALSE, size = 0.5) +
        geom_point(show.legend = FALSE) +
        scale_y_continuous(MODE) +
        scale_x_log10("Aggregation scale [-]",
                      labels = trans_format("log10", math_format(10^.x))) +
        annotation_logticks(sides = "b") +
        theme_bw()+
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank())
    }
  }
}

scalegram_parallel = function(x, MODE, STD=TRUE, threshold=30, cores_used = detectCores() - 1){
  cl = makeCluster(cores_used)
  clusterExport(cl=cl, varlist=c("x", "MODE", "STD", "threshold", "scalegram_main"), envir=environment())
  out  = parApply(cl, as.matrix(x), 2, MODE=MODE, STD=TRUE, threshold=30, scalegram_main)
  return(out)
}

rescale_variance = function(emp_scalegram_coarse, emp_scalegram_fine, scale_ratio){
  rescale_factor = emp_scalegram_fine[scale == scale_ratio]$y_scale
  dummy = emp_scalegram_coarse
  dummy$scale = dummy$scale * scale_ratio
  dummy$y_scale = t(t(dummy$y_scale) * rescale_factor)
  return(dummy)
}

## Theoretical scalegrams
# White noise (WN)
generate_wn = function(sigma, delta){
  stdev = sigma/(sqrt(delta))
  return(stdev ^ 2)
}
# AR(1) process
generate_ar_1 = function(sigma, delta, rho){
  stdev = (sigma/(sqrt(delta))) * sqrt(((1 - rho ^ 2)-(2 * rho * (1 - rho^delta)) / delta)/((1 - rho) ^ 2))
  return(stdev ^ 2)
}
# FGN process
generate_fgn = function(sigma, delta, rho){
  H = 0.5 * (log2(rho + 1) + 1)
  stdev = (delta ^ (H - 1)) * sigma
  return(stdev ^ 2)
}
# Harmonics
generate_harmonic = function(delta, period){
  stdev = (period / (pi * delta)) * abs(sin((pi * delta) / period))
  return(stdev ^ 2)
}


