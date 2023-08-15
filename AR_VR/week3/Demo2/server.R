#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

server <- function(input, output) {
  output$nameOp <- renderText({
    paste("Hello! ", input$name)
  })
  output$ageOp <- renderText({
    paste("It must be thrilling to be ", input$age, " years young!")
  })
  output$countryOp <- renderText({
    paste("I believe ", input$country, " must be a beautiful country. Wish I could visit it.")
  })
}
