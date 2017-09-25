## Empirical scalegrams

scalegram_main <- function(x){
  library(data.table)   # load libraries inside for the 'parallel' package
  nna <- sum(!is.na(x)) # actual length without accounting for missing values
  delta <- round(0.1 * nna, 0)   # aggregrion scale up to 10% of the sample size does not count NAs

  if (delta!=0){
    vardf <- data.frame(scale=1:delta, var_scale=NA)  # create a data.frame to store all the variances
    samp <-data.table(scale(x, center = TRUE, scale = TRUE))     # standardize all data points
    vardf[1,"var_scale"]    <- var(samp, na.rm=T)

    for (i in 2:delta){ # loop around aggregation scale
      dummy <- rep(1:floor(nna/i), each=i)
      ttest <- samp[1:length(dummy), list(AVERAGE=mean(V1)), by=list(dummy)]$AVERAGE

      if (sum(!is.na(ttest))>9){ # have at least 10 values for the variance estimation
        dummyvar <- var(ttest, na.rm=T)
        vardf[i,"var_scale"] <- dummyvar
      }
    } # loop around aggregation scale

  }else{
    vardf <- data.frame(scale=NA, var_scale=NA)
  }
  return(vardf[complete.cases(vardf),])
}

scalegram_parallel = function(x, cores_used = detectCores() - 1){
  cl = makeCluster(cores_used)
  clusterExport(cl=cl, varlist=c("x", "scalegram_main"), envir=environment())
  out  = parApply(cl, as.matrix(x), 2, scalegram_main)
  return(out)
}

rescale_variance = function(emp_scalegram_coarse, emp_scalegram_fine, scale_ratio){
  rescale_factor = emp_scalegram_fine[scale == scale_ratio]$var_scale
  dummy = emp_scalegram_coarse
  dummy$scale = dummy$scale * scale_ratio
  dummy$var_scale = t(t(dummy$var_scale) * rescale_factor)
  return(dummy)
}

## Plot scalegram
plot_scalegram = function(X){
  if(is.ts(X)) {X = scalegram_main(as.vector(X))
  } else if(is.vector(X)) {X = scalegram_main(X)
  }
  if("variable" %in% colnames(X)){

    if(length(unique(X$variable))>10){
    aa = 0.2
    } else {
    aa=1
    }
    ggplot(data=X, aes(x=scale, y=var_scale))+
      geom_line(aes(group=interaction(variable),
                    colour = variable), show.legend = FALSE, size = 0.5, alpha = aa) +
      geom_point(aes(group=interaction(variable),
                     colour=variable), show.legend = FALSE, alpha = aa) +
      geom_tile(aes(fill=variable))+
      scale_y_log10("Variance [-]",
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_x_log10("Aggregation scale [-]",
                    labels = trans_format("log10", math_format(10^.x))) +
      annotation_logticks(sides = "bl") +
      theme_bw()+
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())+
      coord_fixed()
  } else {
    ggplot(data=X, aes(x=scale, y=var_scale))+
      geom_line(show.legend = FALSE, size = 0.5) +
      geom_point(show.legend = FALSE) +
      scale_y_log10("Variance [-]",
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_x_log10("Aggregation scale [-]",
                    labels = trans_format("log10", math_format(10^.x))) +
      annotation_logticks(sides = "bl") +
      theme_bw()+
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())+
      coord_fixed()
  }
}

## Theoretical scalegrams
# White noise (WN)
generate_wn = function(sigma, delta){
  stdev = sigma/(sqrt(delta))
  return(stdev^2)
}
# AR(1) process
generate_ar_1 = function(sigma, delta, rho){
  stdev = (sigma/(sqrt(delta))) * sqrt(((1-rho^2)-(2*rho*(1-rho^delta))/delta)/((1-rho)^2))
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
  stdev = (period/(pi*delta))*abs(sin((pi*delta)/period))
  return(stdev^2)
}


