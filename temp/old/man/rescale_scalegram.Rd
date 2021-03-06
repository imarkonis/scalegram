% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rescale.R
\name{rescale_scalegram}
\alias{rescale_scalegram}
\title{Rescales variance}
\usage{
rescale_scalegram(scalegram_coarse, scalegram_fine, scale_ratio, stat)
}
\arguments{
\item{scalegram_coarse}{A vector, time series or a matrix.}

\item{scalegram_fine}{the statistic used.}

\item{scale_ratio}{the ratio between coarser and finer scale.}

\item{stat}{the statistic used.}
}
\value{
A list with the scalegram of \code{x} for statistic \code{MODE} and the corresponding plot [ggplot object].
}
\description{
Rescales variance
}
\examples{
site_a = owda[owda$Lat == 46.25 & owda$Lon == 16.5,]$scPDSI
site_b = owda[owda$Lat == 46.25 & owda$Lon == 16.5 & owda$Time \%in\% seq(992, 2012, 2) ,]$scPDSI
sg_a = scalegram(site_a)
sg_b = scalegram(site_b)
sg_a$scalegram_plot +
geom_line(data=sg_b$scalegram_df, col = "red") +
geom_point(data=sg_b$scalegram_df, col = "red")

 \\item{"mean" for mean,}
 \\item{"cv" for coefficient of variance,}
}
