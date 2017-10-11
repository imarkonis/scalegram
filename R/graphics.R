## Plot scalegram ==============================================================
##==============================================================================
plot_scalegram = function(X, ...){

  '%!in%' <- function(x,y)!('%in%'(x,y)) # keep function inside for the 'parallel' package

  # check if correct MODE is provided
  if(MODE %!in% c("mu", "s1", "s2", "s3", "s4", "CV",
                  "L1", "L2", "t2", "t3", "t4")){
    return("Error: Invalid MODE. Select one of s1, s2, s3, s4, CV, L1, L2, t2, t3, t4")
  } else {

    if(is.ts(X)) {X = scalegram(as.vector(X), MODE)
    } else if(is.vector(X)) {X = scalegram(X, MODE)
    }
    if("variable" %in% colnames(X)){

      if(length(unique(X$variable))>10){
        aa = 0.2
      } else {
        aa = 1
      }
      ggplot(data=X, aes(x=scale, y=y_scale))+
        geom_line(aes(group=interaction(variable),
                      colour = variable), show.legend = FALSE, size = 0.5, alpha = aa) +
        geom_point(aes(group=interaction(variable),
                       colour=variable), show.legend = FALSE, alpha = aa) +
        geom_tile(aes(fill=variable))+

        scale_y_continuous(MODE) +
        scale_x_log10("Aggregation scale [-]",
                      labels = trans_format("log10", math_format(10^.x))) +
        annotation_logticks(sides = "b") +
        theme_bw()+
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank())
      #      coord_fixed()
    } else {
      ggplot(data=X, aes(x=scale, y=y_scale))+
        geom_line(show.legend = FALSE, size = 0.5) +
        geom_point(show.legend = FALSE) +
        scale_y_continuous(MODE) +
        scale_x_log10("Aggregation scale [-]",
                      labels = trans_format("log10", math_format(10^.x))) +
        annotation_logticks(sides = "b") +
        theme_bw()+
        theme(panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank())
    }
  }
}
