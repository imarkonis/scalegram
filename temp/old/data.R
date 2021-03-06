#' @title Reconstructed summer drought for Czech Republic
#'
#' @description Grided reconstruction (0.5 x 0.5 deg) of the self-calibrated
#' Palmer Drought Index for summer (JJA) precipitation over Europe, as presented
#' by Cook et al. [2015] and edited by Markonis et al. [2017]. Here, only the
#' area for Czech Republic is used (49 - 51 deg N and 13 - 17 E).
#'
#' @references Cook, E. R., Seager, R., Kushnir, Y., Briffa, K. R., Büntgen, U., Frank, D., ... & Baillie, M. (2015).
#' Old World megadroughts and pluvials during the Common Era. Science advances, 1(10).
#'
#' Markonis, Y., Hanel, M., Maca, P., Kysely, Y., & Cook E. (2017). Hydroclimatic conditions of Europe in a millennial perspective.
#' Nature Communications, X(X).
#'
#' @examples
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

"owda"
