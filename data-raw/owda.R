#' @title Reconstructed mean summer scPDSI for Europe
#' @description Estimates scalegram(s) of specific statistics and plots the result.
#' @details Here are the details.
#' @examples
#' scalegram(owda, "s2")
#' scalegram(dataset[,1], "s2")
#' @export

library(data.table)
owda <- data.table(read.csv(file = 'owda.csv'))
devtools::use_data(owda)
