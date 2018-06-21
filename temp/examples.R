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



thres <- 50
x <- brick("merra_tp_-10-50E_30-70N.nc")
no_layer <- nlayers(x)
out <- list()
for(j in 1:no_layer){
  i <- 2
  x_layer <- x[[j]]
  x_layer[,] <- scale(x_layer[,], center = T, scale = T)
  ncells <- length(x_layer)
  x_agg <- list(x_layer)
  while(ncells > thres){
    x_agg[[i]] <- aggregate(x_layer, fact = i)
    ncells <- length(x_agg[[i]])
    i <- i + 1
    print(paste0(i, ".", j))
  }
  out[[j]] <- sapply(sapply(x_agg, getValues), sd, na.rm = T)
}

sc_test <- cbind(rep(1:length(out[[1]]), scale = length(out)), melt(out))
colnames(sc_test)[3] <- "variable"
scalegram_multiplot(sc_test)



