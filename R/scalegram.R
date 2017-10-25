#' @title Determine and plot scalegram
#' @description Estimates scalegram(s) of specific statistics and plots the result.
#' @param x A vector, time series or a matrix.
#' @param stat The statistic used (see Details).
#' @param std If TRUE (default) standardize the scalegram to unit.
#' @param threshold Sample size at last scale (see Details).
#' @param plot If TRUE (default) the scalegram is also plotted.
#' @return A list with the scalegram of \code{x} for statistic \code{stat} and the corresponding plot [ggplot object].
#' @details Here are the details.
#' @examples
#' scalegram(dataset, "s1")
#' scalegram(dataset[,1], "s2")
#' @export

scalegram <-
  function(x, stat = "s1", std = TRUE, threshold = 30, plot = TRUE){
    if (!is.numeric(x)) stop ("x should be numeric.")
    if (!is.vector(x) & !is.ts(x) & !is.matrix(x)) stop ("x should be either vector, or time series, or matrix object.")
    if (is.ts(x)) x = as.vector(x)
    if (is.vector(x)) {
      out = scalegram_main(x, stat, std, threshold)
      out = out[complete.cases(out),]
      out$Variable = "variable"
    }
    else {
      out = scalegram_parallel(x, stat, std, threshold)
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
    if (plot == TRUE){

      if(length(unique(out$Variable))>10){
        transp = 1/log(length(unique(out$Variable)))
      } else {
        transp = 1
      }
      plot_sc = plot_scalegram(out, stat, transparancy=transp)
      print(plot_sc)
      # return only complete cases
      return(list(scalegram_df = out,
                  scalegram_plot = plot_sc))
    }
    else {
      return(out)
    }
  }
