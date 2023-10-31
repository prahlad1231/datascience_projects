# Create an interactive user interface that displays content in a sidebar layout of the attached ds_shinyappset, where 
# the sidebar  panel is on the left and the output plots is on the right.

# reading the file "ds_shinyapp.csv"
ds_shinyapp <- read.csv("ds_shinyapp.csv")

# Load necessary libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(htmltools)

# Define the UI
ui <- fluidPage(
  titlePanel("Final Assessment - Prahlad Panthi (u3248319)"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("categories", "Select Categories:",
                         choices = c("HIGH_HAND", "HIGH_LEG", "LOW_HAND", "LOW_LEG"),
                         selected = "HIGH_HAND"),
      hr(),
      sliderInput("experiment_id", "Select Experiment IDs:", min = 60, max = 100, value = c(60, 100)),
      sliderInput("HIGH_HAND_range", "Select range for HIGH_HAND:", min = 6, max = 10, value = c(6, 10)),
      sliderInput("HIGH_LEG_range", "Select range for HIGH_LEG:", min = 7, max = 10, value = c(6, 10)),
      sliderInput("LOW_HAND_range", "Select range for LOW_HAND:", min = 1, max = 6, value = c(1, 6)),
      sliderInput("LOW_LEG_range", "Select range for LOW_LEG:", min = 1, max = 7, value = c(1, 7))
    ),
    mainPanel(
      plotOutput("boxplot"),
      textOutput("summary")
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to filter data based on user input
  filteredData <- reactive({
    data_filtered <- ds_shinyapp[ds_shinyapp$Experiment_ID %in% input$experiment_id & 
                            ds_shinyapp$HIGH_HAND >= input$HIGH_HAND_range[1] & ds_shinyapp$HIGH_HAND <= input$HIGH_HAND_range[2] &
                            ds_shinyapp$HIGH_LEG >= input$HIGH_LEG_range[1] & ds_shinyapp$HIGH_LEG <= input$HIGH_LEG_range[2] &
                            ds_shinyapp$LOW_HAND >= input$LOW_HAND_range[1] & ds_shinyapp$LOW_HAND <= input$LOW_HAND_range[2] &
                            ds_shinyapp$LOW_LEG >= input$LOW_LEG_range[1] & ds_shinyapp$LOW_LEG <= input$LOW_LEG_range[2], ]
    return(data_filtered)
  })
  
  
  # Boxplot output

  output$boxplot <- renderPlot({
    ggplot(filteredData() %>% pivot_longer(cols = c(input$categories), names_to = "category", values_to = "value"), aes(x = category, y = value, fill = category)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
      scale_fill_manual(values = c("red", "green", "blue", "orange")) +  # Define colors here
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line.x = element_line(color = "black", size = 0.5),
            axis.line.y = element_line(color = "black", size = 0.5)
      )
  })
  
  # Summary output
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
