\name{scalegram}
\alias{scalegram}
\title{Estimate and plot scalegram}
\usage{
scalegram(x, stat = "var", std = TRUE, threshold = 30, plot = TRUE, fast = FALSE, ...)
}

\description{
The function \code{scalegram} computes (and by default plots) the aggregation curve of a given statistic in a single dimension.
}

\arguments{
\item{x}{A numeric vector.}

\item{stat}{The statistic which will be estimated across the cross-scale continuum. Suitable options are:
\itemize{
 \item{"var" for variance,}
 \item{"sd" for standard deviation,}
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

\item{...}{log_x and log_y (default TRUE) for setting the axes of the scalegram plot to logarithmic scale. The argument wn (default FALSE) is used to plot a line presenting the standardized variance of the white noise process. Therefore, it should be used only with stat = "var" and std = T.}
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
scalegram(rnorm(1000), wn = T)

data(gpm_nl, knmi_nl, rdr_nl, ncep_nl, cnrm_nl, gpm_events)
scalegram(knmi_nl$prcp, threshold = 10, fast = T)

scalegram(gpm_nl$prcp, stat = "skew", std = F, log_x = F, log_y = F, smooth = T)

gpm_skew <- scalegram(gpm_nl$prcp, stat = "skew", std = F, log_x = F, log_y = F, smooth = T, plot = F)
rdr_skew <- scalegram(rdr_nl$prcp, stat = "skew", std = F, log_x = F, log_y = F, smooth = T, plot = F)
scalegram_multiplot(rbind(data.frame(gpm_skew, dataset = "gpm"), data.frame(rdr_skew, dataset = "rdr")), log_x = F, log_y = F, smooth = T)

set_1 <- data.frame(scalegram(gpm_nl$prcp, plot = F, fast = T), dataset = "gpm")
set_2 <- data.frame(scalegram(rdr_nl$prcp, plot = F, fast = T), dataset = "radar")
set_3 <- data.frame(scalegram(knmi_nl$prcp, plot = F, fast = T), dataset = "station")
set_4 <- data.frame(scalegram(ncep_nl$prcp, plot = F, fast = T), dataset = "ncep")
set_5 <- data.frame(scalegram(cnrm_nl$prcp, plot = F, fast = T), dataset = "cnrm")
scalegram_multiplot(rbind(set_1, set_2, set_3, set_4, set_5))




}
\references{
Markonis, Y., & Koutsoyiannis, D. (2013). Climatic variability over time scales spanning nine orders of magnitude:
Connecting Milankovitch cycles with Hurst–Kolmogorov dynamics. Surveys in Geophysics, 34(2), 181-207.

Pappas, C., Mahecha, M. D., Frank, D. C., Babst, F., & Koutsoyiannis, D. (2017).
Ecosystem functioning is enveloped by hydrometeorological variability. Nature Ecology & Evolution, 1(9), 1263.
}




