library(shiny)
library(rgl)

data("iris")

ui <- fluidPage(
  titlePanel("My Take Home Shiny App!"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "choice",
        label = "Select variable to plot:",
        choices = colnames(iris[colnames(iris)])
      )
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    hist(iris[[input$choice]])
    xlab=input$choice
  })
}

shinyApp(ui=ui, server = server)