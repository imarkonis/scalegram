test_dt = data.table(cbind(variable = sample(c(0,1), size = 1000, replace = T), value= rnorm(500)))
test_dt_scale = test_dt[, scalegram(value), variable]

plot_scalegram(test_dt_scale)
