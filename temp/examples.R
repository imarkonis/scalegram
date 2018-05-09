test <- owda
test[, variable := .GRP, by = .(Lat, Lon)]
tt = tapply(test$scPDSI, test$variable, scalegram, plot = F, simplify = T)
tt = mapply(cbind, tt, "variable" = names(tt), SIMPLIFY=F)
tt = do.call(rbind.data.frame, tt)
tt$scale = as.numeric(as.character(tt$scale))
tt$var = as.numeric(as.character(tt$var))

require(ncdf4)
require(raster)
require(data.table)
require(reshape2)

transform.dt.to.brick = function(my.dt, variable.name) {
array.from.dt = acast(my.dt, lat ~ lon ~ time, value.var = variable.name)
my.brick = brick(array.from.dt, ymn = min(as.numeric(rownames(array.from.dt))),
ymx = max(as.numeric(rownames(array.from.dt))), xmn = min(as.numeric(colnames(array.from.dt))),
xmx = max(as.numeric(colnames(array.from.dt))))
my.brick = flip(my.brick, direction = "2")
return(my.brick)
}

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
scalegram_plot(aabb)



