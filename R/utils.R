rescale_variance = function(emp_scalegram_coarse, emp_scalegram_fine, scale_ratio){
  rescale_factor = emp_scalegram_fine[scale == scale_ratio]$y_scale
  dummy = emp_scalegram_coarse
  dummy$scale = dummy$scale * scale_ratio
  dummy$y_scale = t(t(dummy$y_scale) * rescale_factor)
  return(dummy)
}

## Theoretical scalegrams
# White noise (WN)
generate_wn = function(sigma, delta){
  stdev = sigma/(sqrt(delta))
  return(stdev ^ 2)
}
# AR(1) process
generate_ar_1 = function(sigma, delta, rho){
  stdev = (sigma/(sqrt(delta))) * sqrt(((1 - rho ^ 2)-(2 * rho * (1 - rho^delta)) / delta)/((1 - rho) ^ 2))
  return(stdev ^ 2)
}
# FGN process
generate_fgn = function(sigma, delta, rho){
  H = 0.5 * (log2(rho + 1) + 1)
  stdev = (delta ^ (H - 1)) * sigma
  return(stdev ^ 2)
}
# Harmonics
generate_harmonic = function(delta, period){
  stdev = (period / (pi * delta)) * abs(sin((pi * delta) / period))
  return(stdev ^ 2)
}
