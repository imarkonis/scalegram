#' Main scalegram function
#'
#' @param x A vector, time series or a matrix.
#' @param MODE the statistic used.
#' @return A list with the scalegram of \code{x} for statistic \code{MODE} and the corresponding plot [ggplot object].
#' @examples
#' scalegram(dataset, "s1")
#' scalegram(dataset[,1], "s2")
#' @export

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
