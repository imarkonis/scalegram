require(ncdf4)
require(raster)
require(data.table)
require(reshape2)
require(ggplot2)

transform.dt.to.brick = function(my.dt, variable.name) {
  array.from.dt = acast(my.dt, lat ~ lon ~ time, value.var = variable.name)
  my.brick = brick(array.from.dt, ymn = min(as.numeric(rownames(array.from.dt))),
                   ymx = max(as.numeric(rownames(array.from.dt))), xmn = min(as.numeric(colnames(array.from.dt))),
                   xmx = max(as.numeric(colnames(array.from.dt))))
  my.brick = flip(my.brick, direction = "2")
  return(my.brick)
}


####OWDA
test <- readRDS("../OWDA/Data/OWDA_1000.rds")
colnames(test) = c("time", "lat", "lon", "scPDSI")
owda_nc = transform.dt.to.brick(test, variable.name = "scPDSI")

owda_nc_1001 <- owda_nc$X1001
owda_nc_1005 <- owda_nc$X1005
owda_nc_rnorm <- owda_nc$X1005
owda_nc_rnorm[,] <- rnorm(length(owda_nc_rnorm[,]), mean = 1, sd = 1.5)

ncells <- length(owda_nc_1001)
owda_nc_1001_agg = list(owda_nc_1001)
owda_nc_1005_agg = list(owda_nc_1005)
owda_nc_rnorm_agg = list(owda_nc_rnorm)
i <- 2
while(ncells > 50){
  owda_nc_1001_agg[[i]] <- aggregate(owda_nc_1001, fact = i)
  owda_nc_1005_agg[[i]] <- aggregate(owda_nc_1005, fact = i)
  owda_nc_rnorm_agg[[i]] <- aggregate(owda_nc_rnorm, fact = i)
  ncells <- length(owda_nc_1001_agg[[i]])
  ncells <- length(owda_nc_1005_agg[[i]])
  ncells <- length(owda_nc_rnorm_agg[[i]])
  i <- i + 1
  print(i)
}

aa <- sapply(sapply(owda_nc_1001_agg, getValues), sd, na.rm = T)
bb <- sapply(sapply(owda_nc_1005_agg, getValues), sd, na.rm = T)

######NASA MERRA PRCP
test <- brick("merra_tp_-10-50E_30-70N.nc")
test_x7 <- test$X7
test_x7[,] <- scale(test_x7[,], center = T, scale = T)

test_x10 <- test$X10
test_x10[,] <- scale(test_x10[,], center = T, scale = T) #For monthly (mm) * 3600 * 24 * 30

ncells <- length(test_x7)
test_7_agg = list(test_x7)
test_10_agg = list(test_x7)

i <- 2
while(ncells > 50){
  test_7_agg[[i]] <- aggregate(test_x7, fact = i)
  test_10_agg[[i]] <- aggregate(test_x10, fact = i)
  ncells <- length(test_7_agg[[i]])
  ncells <- length(test_10_agg[[i]])

  i <- i + 1
  print(i)
}

aa <- sapply(sapply(test_7_agg, getValues), sd, na.rm = T)
bb <- sapply(sapply(test_10_agg, getValues), sd, na.rm = T)

aa_mat <- cbind(1:14, aa)
bb_mat <- cbind(1:14, bb)
scalegram_plot(aa_mat)

aabb <- rbind(cbind(aa_mat, 7), cbind(bb_mat, 10))





