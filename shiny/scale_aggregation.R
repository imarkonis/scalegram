library(shiny)
library(lattice)

agg = function(x, agg.scale, FUN = mean) {
  aa = as.numeric(tapply(x, (seq_along(x) - 1) %/% agg.scale, FUN, na.rm = T))
  return(aa[1:(length(aa)-1)])
}

ui <- fluidPage(
  numericInput(inputId = "scale_1", label = "Agg. scale 1:", value = 10),
  numericInput(inputId = "scale_2", label = "Agg. scale 2:", value = 100),
  sliderInput(inputId = "H",
              step = 0.01,
              label = "Hurs coef.:",
              min = 0,
              max = 1,
              value = 0.5),
  plotOutput("aggregation")
)

server <- function(input, output) {

  output$aggregation <- renderPlot({
    randomVals <- FGN::SimulateFGN(1000, input$H)
    agg_1 <- agg(randomVals, input$scale_1, mean)
    agg_1 <- data.frame(cbind(scale = seq(input$scale_1/2, 1000, input$scale_1), value = agg_1))
    agg_2 <- agg(randomVals, input$scale_2, mean)
    agg_2 <- data.frame(cbind(scale = seq(input$scale_2/2, 1000, input$scale_2), value = agg_2))

    xyplot(randomVals~1:1000, col = "gray70", type = 'b', pch = 15, cex = 0.8) +
    xyplot(value~scale, agg_1, col = "dark red", type = 'b', pch = 16, lwd = 2, cex = 1.1) +
      xyplot(value~scale, agg_2, col = "grey30", type = 'b', pch = 16, lwd = 2, cex = 2)
  })
}

shinyApp(ui, server)



output$extremes = renderPlot({
  lim.a = input$year - 50
  lim.b = input$year + 50
  xyplot(ratio~Time, oo.1[oo.1$Time > lim.a & oo.1$Time < lim.b],
         pch = 19, type=c("b", "g"), col = pluvial.col(30)[1],
         xlab = list(label = "Time (year CE)", cex = 1.2),
         ylab = list(label = "Grid cells percentage", cex = 1.2),
         cex = 1, lwd = 2, alpha=0.7,
         scales = list(y = list(tck=c(-1, 0)),
                       x = list(tck=c(-1, 0))))+
    xyplot(rep(0.05, 2)~c(-100,100), col = "black", lty = 2, lwd = 1.5, type = 'l')+
    xyplot(c(0.05,0.05)~c(0,3000), type = 'l', lty = 2, col = "gray20")+

    xyplot(ratio~Time, oo.10[oo.10$Time > lim.a & oo.10$Time < lim.b],
           pch = 19, type="b", col = pluvial.col(100)[50],
           xlab = list(label = "Time (year CE)", cex = 1.2),
           ylab = list(label = "Grid cells percentage", cex = 1.2),
           cex = 2, lwd = 2, alpha=0.7,
           scales = list(y = list(tck=c(-1, 0)),
                         x = list(tck=c(-1, 0))))+

    xyplot(ratio~Time, oo.30[oo.30$Time > lim.a & oo.30$Time < lim.b],
           pch = 19, type="b", col = pluvial.col(100)[100],
           xlab = list(label = "Time (year CE)", cex = 1.2),
           ylab = list(label = "Grid cells percentage", cex = 1.2),
           cex = 3, lwd = 2, alpha=0.7,
           scales = list(y = list(tck=c(-1, 0)),
                         x = list(tck=c(-1, 0))))
})
}
