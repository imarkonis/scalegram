\name{scalegram}
\alias{scalegram}
\title{Estimate and plot scalegram}
\usage{
scalegram(x, stat = "sd", std = TRUE, threshold = 30, plot = TRUE, fast = FALSE, ...)
}

\description{
The function \code{scalegram} computes (and by default plots) estimates of the scaling behaviour of given statistics.
}

\arguments{
\item{x}{A numeric vector, time series or matrix.}

\item{stat}{The statistic which will be estimated across the cross-scale continuum. Suitable options are:
\itemize{
 \item{"sd" for standard deviation,}
 \item{"var" for variance,}
 \item{"skew" for skewness,}
 \item{"kurt" for kurtosis,}
 \item{"l2" for L-scale,}
 \item{"t2" for coefficient of L-variation,}
 \item{"t3" for L-skewness,}
 \item{"t4" for L-kurtosis.}
}}

\item{std}{logical. If TRUE (the default) the scalegram is standardized to unit, i.e., zero mean and unit variance in the original time scale.}

\item{threshold}{numeric. Sample size of the time series at the last aggregated scale (see Details).}

\item{plot}{logical. If TRUE (the default) the scalegram is plotted.}

\item{fast}{logical. If TRUE the scalegram is estimated only in logarithmic scale; 1, 2, 3, ... , 10, 20, 30, ... , 100, 200, 300 etc.}

\item{...}{log_x and log_y (default TRUE) for setting the axes of the scalegram plot to logarithmic scale.}
}

\value{
If \code{plot = TRUE}, the \code{scalegram} returns a list containing:
\itemize{
 \item{\code{sg_data}: Matrix of the timeseries values for the selected \code{stat} at each \code{scale}.
 \item{\code{sg_plot}: Plot of \code{scale} versus \code{stat} as a \emph{ggplot} object.}
}
If \code{plot = FALSE}, then it returns only the matrix of the timeseries values for the selected \code{stat} at each \code{scale}.
}

\examples{
scalegram(rnorm(1000))

## Plot scalegram in logarithmic y axis
data(owda)
owda_site <- owda[owda$Lat == 50.25 & owda$Lon == 16.5, ]$scPDSI
sgram_sd <- scalegram(owda_site, "l2")
sgram_sd$sg_plot + scale_y_log10("L-scale")

## Plot scalegram from tidy format
owda_mat <- acast(owda, Time ~ Lon + Lat, value.var = "scPDSI") #transform them into matrix
sgram_mat_sd = scalegram(owda_mat, std = F, threshold = 50)

## Plot two scalegrams in a single plot
site_a <- owda[owda$Lat == 50.25 & owda$Lon == 16.5, ]$scPDSI
site_b <- owda[owda$Lat == 50.75 & owda$Lon == 16.5, ]$scPDSI
aa <- scalegram(site_a)
bb <- scalegram(site_b)
aa$sg_plot +
geom_line(data=bb$sg_df, col = "red") +
geom_point(data=bb$sg_df, col = "red")

 plot_scalegram(scalegram(rnorm(100000), fast = T, plot = F), wn = T)
}
\references{
Markonis, Y., & Koutsoyiannis, D. (2013). Climatic variability over time scales spanning nine orders of magnitude:
Connecting Milankovitch cycles with Hurst–Kolmogorov dynamics. Surveys in Geophysics, 34(2), 181-207.

Pappas, C., Mahecha, M. D., Frank, D. C., Babst, F., & Koutsoyiannis, D. (2017).
Ecosystem functioning is enveloped by hydrometeorological variability. Nature Ecology & Evolution, 1(9), 1263.
}
