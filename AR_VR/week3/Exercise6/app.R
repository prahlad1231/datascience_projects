library(shiny)
library(rgl)

set.seed(100)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", label="n", min=10, max=100, value=10, step=10)
    ),
    mainPanel(
      rglwidgetOutput("myPlot", width = 800, height = 600)
    )
  )
)

server <- function(input, output) {
  output$myPlot <- renderRglwidget({
    n <- input$n
    rgl.open(useNULL = T)
    scatter3d(rnorm(n), rnorm(n), rnorm(n))
    rglwidget()
  })
}

shinyApp(ui=ui, server = server)