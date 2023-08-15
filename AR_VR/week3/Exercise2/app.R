library(shiny)

country_list <- list("Nepal", "India", "Australia", "USA", "Others")

ui <- fluidPage(
  titlePanel("HTML Demo"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter your name"),
      numericInput("age", "Enter your age", value=1, min = 1, max = 100, step=1),
      selectInput("country", "Select your country", choices = country_list)
    ),
    mainPanel(
      h4("Personal Data"),
      p("This is your ", strong("personal data")),
      textOutput("nameOp"),
      textOutput("ageOp"),
      textOutput("countryOp")
    )
  )
)

server <- function(input, output) {
  output$nameOp <- renderText({
    paste("Hello! ", input$name)
  })
  output$ageOp <- renderText({
    paste("It must be thrilling to be ", input$age, " years young!")
  })
  output$countryOp <- renderText({
    paste("I believe ", input$country, " must be a beautiful country.")
  })
}

shinyApp(ui=ui, server = server)
