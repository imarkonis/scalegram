library(shiny)
library(scalegram)

ui <- fluidPage(
  actionButton("go", "Go"),
  numericInput("n", "n", 1000),
  sliderInput(inputId = "H",
              step = 0.01,
              label = "Hurs coef.:",
              min = 0,
              max = 1,
              value = 0.75),
  plotOutput("scalegram")
)

server <- function(input, output) {
  randomVals_a <- eventReactive(input$go, {
    rnorm(input$n)
  })

  randomVals_b <- eventReactive(input$go, {
    FGN::SimulateFGN(input$n, input$H)
  })

  output$scalegram <- renderPlot({
    aa <- scalegram::scalegram(randomVals_a())
    bb <- scalegram::scalegram(randomVals_b())
    aa$sg_plot +
      geom_line(data=bb$sg_df, col = "red") +
      geom_point(data=bb$sg_df, col = "red") +
      scale_y_log10()
  })
}

shinyApp(ui, server)

