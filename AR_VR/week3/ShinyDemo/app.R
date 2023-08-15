library(shiny)

ui <- fluidPage(
  titlePanel("Shiny App Demo"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter your name: ", "your name...")
    ),
    mainPanel(textOutput("txtOutput"))
  )
)
server <- function(input, output){
  output$txtOutput <- renderText({
    paste("Your name is ", input$name)
  })
}
shinyApp(ui=ui, server = server)
