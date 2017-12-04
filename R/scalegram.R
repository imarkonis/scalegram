#' @title Estimate and plot scalegram
#' @description The function \code{scalegram} computes (and by default plots) estimates of the scaling behaviour of given statistics.
#' @param x A numeric vector, time series or matrix.
#' @param stat The statistic which will be estimated across the cross-scale continuum. Suitable options are:
#' \itemize{
#'  \item{"sd" for standard deviation,}
#'  \item{"var" for variance,}
#'  \item{"skew" for skewness,}
#'  \item{"kurt" for kurtosis,}
#'  \item{"l2" for L-scale,}
#'  \item{"t2" for coefficient of L-variation,}
#'  \item{"t3" for L-skewness,}
#'  \item{"t4" for L-kurtosis.}
#' }
#' @param std logical. If TRUE (the default) the scalegram is standardized to unit,
#' i.e., zero mean and unit variance in the original time scale.
#' @param threshold numeric. Sample size of the time series at the last aggregated
#' scale (see Details).
#' @param plot logical. If TRUE (the default) the scalegram is plotted.
#'
#' @details If \code{x} is a matrix, then \code{scalegram} will incorporate a
#' parallel computing procedure to increase resources efficiency.
#'
#' @return  If \code{plot = TRUE}, the \code{scalegram} returns a list containing:
#' \itemize{
#'  \item{\code{sg_df}: Data frame of the timeseries values for the selected \code{stat} at each \code{scale}.
#'   If \code{x} is a matrix then the column named \code{variable} corresponds to the matrix column names.}
#'  \item{\code{sg_plot}: The plot of \code{scale} versus \code{stat} as a \emph{ggplot} object.}
#' }
#' If \code{plot = FALSE}, then it returns only the first element of the list as a data.frame.
#'
#' @references
#'
#' Markonis, Y., & Koutsoyiannis, D. (2013). Climatic variability over time scales spanning nine orders of magnitude :
#' Connecting Milankovitch cycles with Hurst–Kolmogorov dynamics. Surveys in Geophysics, 34(2), 181-207.
#'
#' Pappas, C., Mahecha, M. D., Frank, D. C., Babst, F., & Koutsoyiannis, D. (2017).
#' Ecosystem functioning is enveloped by hydrometeorological variability. Nature Ecology & Evolution, 1(9), 1263.
#'
#' @examples
#' scalegram(rnorm(1000))
#' scalegram(replicate(100, rnorm(3000)))
#'
#' ## Plot scalegram in logarithmic y axis
#' data(owda)
#' owda_site <- owda[owda$Lat == 50.25 & owda$Lon == 16.5, ]$scPDSI
#' sgram_sd <- scalegram(owda_site, "l2")
#' sgram_sd$sg_plot + scale_y_log10("L-scale")
#'
#' ## Plot scalegram from tidy format
#' owda_mat <- acast(owda, Time ~ Lon + Lat, value.var = "scPDSI") #transform them into matrix
#' sgram_mat_sd = scalegram(owda_mat, std = F, threshold = 50)
#'
#' ## Plot two scalegrams in a single plot
#' site_a <- owda[owda$Lat == 50.25 & owda$Lon == 16.5, ]$scPDSI
#' site_b <- owda[owda$Lat == 50.75 & owda$Lon == 16.5, ]$scPDSI
#' aa <- scalegram(site_a)
#' bb <- scalegram(site_b)
#' aa$sg_plot +
#' geom_line(data=bb$sg_df, col = "red") +
#' geom_point(data=bb$sg_df, col = "red")
#'
#' @export

scalegram  <- function(x, stat = "sd", std = TRUE, threshold = 30, plot = TRUE) {
    if (!is.numeric(x)) stop ("x should be numeric.")
    if (!is.vector(x) & !is.ts(x) & !is.matrix(x))
      stop ("x should be vector, time series or matrix object.")
    if (is.ts(x)) x <- as.vector(x)
    if (is.vector(x)) {
      out <- scalegram_main(x, stat, std, threshold)
      out <- out[complete.cases(out),]
      colnames(out)[2] = stat
    }
    else {
      if(is.null(colnames(x))){
        colnames(x) = 1:ncol(x)
      }
      out_list <- scalegram_parallel(x, stat, std, threshold)
      out <- do.call(rbind.data.frame, out_list)
      out$variable <- rep(names(out_list), each = nrow(out_list[[1]]))
      rownames(out) = NULL
      colnames(out)[2] = stat
    }

    if (plot == TRUE){
      plot_sc <- plot_scalegram(out)
      return(list(sg_df   = out,
                  sg_plot = plot_sc))
    }
    else {
      return(out)
    }
  }
