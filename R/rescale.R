rescale_scalegram <- function(scalegram_coarse, scalegram_fine, scale_ratio){
  rescale_factor = scalegram_fine[scale == scale_ratio]$value 
  dummy = scalegram_coarse
  dummy$scale = dummy$scale * scale_ratio
  dummy$value = t(t(dummy$value) * rescale_factor)
  return(dummy)
}

