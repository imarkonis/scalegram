list_of_packages = c("data.table", "reshape2", "RColorBrewer", "zoo", "ggplot2", "scales","RNCEP", "gimms", "ncdf4", "parallel", "longmemo", "HKprocess", "e1071")

new_packages = list_of_packages [!(list_of_packages  %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(list_of_packages, require, character.only = TRUE)
