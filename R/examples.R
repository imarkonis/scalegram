test <- owda
test[, variable := .GRP, by = .(Lat, Lon)]
tt = tapply(test$scPDSI, test$variable, scalegram, plot = F, simplify = T)
tt = mapply(cbind, tt, "variable" = names(tt), SIMPLIFY=F)
tt = do.call(rbind.data.frame, tt)
tt$scale = as.numeric(as.character(tt$scale))
tt$var = as.numeric(as.character(tt$var))

scalegram_multiplot(tt)

