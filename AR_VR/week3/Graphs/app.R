library(shiny)

# Exercise 3

ui <- fluidPage(
  tags$h1("Heading 1"),
  tags$hr(),
  tags$p(strong("Prahlad Panthi")),
  tags$p(em("University of Canberra")),
  tags$a(href="https://www.canberra.edu.au/", "Click here")
)

server <- function(input, output) {}

# Exercise 4

ui <- fluidPage(
  numericInput(inputId = "n", "Sample size", value=25),
  plotOutput(outputId = "hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$n))
  })
}


# RUNNING APP
shinyApp(ui = ui, server = server)
 