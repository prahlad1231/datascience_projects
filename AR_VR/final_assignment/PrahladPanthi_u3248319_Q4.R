# Create an interactive user interface that displays content in a sidebar layout of the attached dataset_shinyapp.csv, where 
# the sidebar  panel is on the left and the output plots is on the right.

# reading the file "ds_shinyapp.csv"
ds_shinyapp <- read.csv("dataset_shinyapp.csv")

# Loading necessary libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(htmltools)

# Defining the UI
ui <- fluidPage(
  titlePanel("Final Assessment - Prahlad Panthi (u3248319)"), # setting the title of the panel
  sidebarLayout( # creating a sidebar in the window
    sidebarPanel(
      checkboxGroupInput("categories", "Select Categories:",
                         choices = c("HIGH_HAND", "HIGH_LEG", "LOW_HAND", "LOW_LEG"),
                         selected = "HIGH_HAND"), # setting up the checkboxes with different choices in the sidebar
      hr(),
      # creating a slider input for experiment id with values ranging from 60 to 100 and initially selecting the complete range
      sliderInput("experiment_id", "Select Experiment IDs:", min = 60, max = 100, value = c(60, 100)),
      # creating a slider input for HIGH_HAND with values ranging from 6 to 10 and initially selecting the complete range
      sliderInput("HIGH_HAND_range", "Select range for HIGH_HAND:", min = 6, max = 10, value = c(6, 10)),
      # creating a slider input for HIGH_LEG with values ranging from 7 to 10 and initially selecting the complete range
      sliderInput("HIGH_LEG_range", "Select range for HIGH_LEG:", min = 7, max = 10, value = c(7, 10)),
      # creating a slider input for LOW_HAND with values ranging from 1 to 6 and initially selecting the complete range
      sliderInput("LOW_HAND_range", "Select range for LOW_HAND:", min = 1, max = 6, value = c(1, 6)),
      # creating a slider input for LOW_LEG with values ranging from 1 to 7 and initially selecting the complete range
      sliderInput("LOW_LEG_range", "Select range for LOW_LEG:", min = 1, max = 7, value = c(1, 7))
    ),
    mainPanel(
      plotOutput("boxplot"), # boxplot id is used for displaying the plot output
      textOutput("summary") # summary id is used for displaying the summary
    )
  )
)

server <- function(input, output) {
  
  # Creating a reactive expression to filter data based on user input
  filteredData <- reactive({
    data_filtered <- ds_shinyapp[ds_shinyapp$Experiment_ID %in% input$experiment_id & # making sure that the experiment id from dataset is in the experiment id selected by the user
                            # making sure that the values of HIGH_HAND from the dataset is between the selected range by the user       
                            ds_shinyapp$HIGH_HAND >= input$HIGH_HAND_range[1] & ds_shinyapp$HIGH_HAND <= input$HIGH_HAND_range[2] &
                            # making sure that the values of HIGH_LEG from the dataset is between the selected range by the user
                            ds_shinyapp$HIGH_LEG >= input$HIGH_LEG_range[1] & ds_shinyapp$HIGH_LEG <= input$HIGH_LEG_range[2] &
                            # making sure that the values of LOW_HAND from the dataset is between the selected range by the user
                            ds_shinyapp$LOW_HAND >= input$LOW_HAND_range[1] & ds_shinyapp$LOW_HAND <= input$LOW_HAND_range[2] &
                            # making sure that the values of LOW_LEG from the dataset is between the selected range by the user
                            ds_shinyapp$LOW_LEG >= input$LOW_LEG_range[1] & ds_shinyapp$LOW_LEG <= input$LOW_LEG_range[2], ]
    return(data_filtered)
  })
  
  
  # creating the output for box plot

  output$boxplot <- renderPlot({
    ggplot(filteredData() %>% pivot_longer(cols = c(input$categories), names_to = "category", values_to = "value"), 
      aes(x = category, y = value, fill = category)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
      scale_fill_manual(values = c("red", "green", "blue", "orange")) +  
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line.x = element_line(color = "black", size = 0.5),
            axis.line.y = element_line(color = "black", size = 0.5)
      )
  })
  
  # Creating the summary for the output
  output$summary <- renderText({
    categories_to_summary <- input$categories
    if (length(categories_to_summary) == 0) {
      return(NULL)
    }
    
    summary_data <- filteredData()
    result <- "Statistics:"
    
    for (category in categories_to_summary) {
      category_data <- summary_data[, category]
      mean_val <- mean(category_data)
      sd_val <- sd(category_data)
      
      result <- paste(result, category, "Average:", mean_val, "SD:", sd_val, sep = "\n")
    }
    
    return(result)
  })
}

shinyApp(ui = ui, server = server)

