#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

country_list <- list("Nepal", "India", "Australia", "USA", "Others")

fluidPage(
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
