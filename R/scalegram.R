scalegram  <- function(x, stat = "sd", std = T, threshold = 30, plot = T, ...) {
    if (!is.numeric(x)) stop ("x should be numeric.")
    if (!is.vector(x)) stop ("x should be vector.")
      out <- scalegram_main(x, stat, std, threshold)
      out <- out[complete.cases(out),]
      colnames(out)[2] = "value"
    if (plot == T){
      plot_sc <- plot_scalegram(out, ...)
      return(list(sg_df   = out,
                  sg_plot = plot_sc))
    }
    else {
      return(out)
    }
  }
