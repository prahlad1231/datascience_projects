# Exercise 5
library(shiny)
library(rgl)
#library(plot3D)
library(car)

data("iris")
# creating vectors for plots
x <- iris$Sepal.Length
y <- iris$Petal.Length
z <- iris$Sepal.Width

ui <- fluidPage(
  rglwidgetOutput("plot", width = 800, height = 600)
)

server <- function(input, output) {
  output$plot <- renderRglwidget({
    open3d(useNULL = T)
    scatter3d(x,y,z,groups=iris$Species,
              col= as.numeric(iris$Species), surface=F)
    rglwidget()
  })
}

# RUNNING APP
shinyApp(ui = ui, server = server)

