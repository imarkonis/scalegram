scalegram  <- function(x, stat = "sd", std = TRUE, threshold = 30, plot = TRUE, ...) {
    if (!is.numeric(x)) stop ("x should be numeric.")
    if (!is.vector(x) & !is.ts(x) & !is.matrix(x))
      stop ("x should be vector, time series or matrix object.")
    if (is.ts(x)) x <- as.vector(x)
    if (is.vector(x)) {
      out <- scalegram_main(x, stat, std, threshold)
      out <- out[complete.cases(out),]
      colnames(out)[2] = "value"
      out$variable = "Variable"
    }
    else {
      if(is.null(colnames(x))){
        colnames(x) = 1:ncol(x)
      }
      out_list <- scalegram_parallel(x, stat, std, threshold)
      out <- do.call(rbind.data.frame, out_list)
      out$variable <- rep(names(out_list), each = nrow(out_list[[1]]))
      rownames(out) = NULL
      colnames(out)[2] = "value"
    }

    if (plot == TRUE){
      plot_sc <- plot_scalegram(out, ...)
      return(list(sg_df   = out,
                  sg_plot = plot_sc))
    }
    else {
      return(out)
    }
  }
