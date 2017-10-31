#' @title Estimate and plot scalegram
#' @description Estimates scalegram(s) of specific statistics and plots the result.
#' @param x The dataset given as vector, time series or a matrix.
#' @param stat The statistic which will be estimated across the scale continuum. Possible statistics are:
#' \itemize{
#'  \item{"mean" for mean,}
#'  \item{"sd" for standard deviation,}
#'  \item{"var" for variance,}
#'  \item{"skew" for skewness,}
#'  \item{"kurt" for kurtosis,}
#'  \item{"cv" for coefficient of variance,}
#'  \item{"L2" for L-scale,}
#'  \item{"t2" for coefficient of L-variation,}
#'  \item{"t3" for L-skewness,}
#'  \item{"t4" for L-kurtosis.}
#' }
#' @param std If TRUE (default) standardize the scalegram to unit, i.e., zero mean and unit variance at the original time scale.
#' @param threshold Sample size at the last scale (see Details).
#' @param plot If TRUE (default) the scalegram is also plotted.
#' @return A list containing the scalegram of \code{x} for the given \code{stat} statistic and the corresponding plot as a \emph{ggplot object}.
#' @details Here are the details.
#' @examples
#' scalegram(rnorm(1000))
#'
#' ## Plot scalegram in logarithmic y axis
#' sgram_sd <- scalegram(owda[Lat == 46.25 & Lon == 16.5, scPDSI] , "L2")
#' sgram_sd$scalegram_plot + scale_y_log10("L-scale")
#'
#' ## Plot matrix
#' owda_mat = owda_mat = acast(owda, Time ~ Lon + Lat, value.var = "scPDSI")
#' sgram_mat_sd = scalegram(owda_mat, std = F, threshold = 50)
#'
#'
#'
#' @export


scalegram <-
  function(x, stat = "sd", std = TRUE, threshold = 30, plot = TRUE) {
    if (!is.numeric(x)) stop ("x should be numeric.")
    if (!is.vector(x) & !is.ts(x) & !is.matrix(x))
      stop ("x should be either vector, or time series, or matrix object.")
    if (is.ts(x)) x <- as.vector(x)
    if (is.vector(x)) {
      out = scalegram_main(x, stat, std, threshold)
      out = out[complete.cases(out),]
      out$Variable = "variable"
    }
    else {
      out <- scalegram_parallel(x, stat, std, threshold)
      rows_diff <- max(sapply(out, function(x) max(x[[1]])))
      columns <- length(out)
      out_matr <- matrix(NA, nrow = rows_diff, ncol = columns)
      rownames(out_matr) <- 1:rows_diff
      colnames(out_matr) <- 1:columns
      for (i in 1:columns){
        out_matr[out[[i]]$scale,i] <- out[[i]]$y_scale
      }
      out <- melt(out_matr)
      colnames(out) <- c("scale", "Variable", "y_scale")
      out <- out[complete.cases(out), ]
    }
    if (plot == TRUE){
      if(length(unique(out$Variable)) > 10){
        transp <- 1 / log(length(unique(out$Variable)))
      } else {
        transp <- 1
      }
      plot_sc <- plot_scalegram(out, stat, transparancy = transp)
      print(plot_sc)
      return(list(scalegram_df   = out,
                  scalegram_plot = plot_sc))
    }
    else {
      return(out)
    }
  }
