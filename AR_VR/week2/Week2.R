rm(list= ls())

# load iris dataset
data("iris")
head(iris)

# create vectors for plots
x <- iris$Sepal.Length
y <- iris$Petal.Length
z <- iris$Sepal.Width

# 3D plots with plot3D, first install plot3D package
install.packages("plot3D")
capabilities("tcltk")
library("plot3D")

###################### Exercise 1: 3-D plots #######################
scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"))

# remove color bar
scatter3D(x, y, z, colkey = F)

# change the style
# colvar: color of the points
# col: color the points
# pch: point shape
# cex: size of the points
scatter3D(x,y,z, colvar = NULL, col = "blue", pch=19, cex=0.9)

# full box
scatter3D(x,y,z, bty = "f", colkey = F, main="bty=f")

# grey background with white grid lines and tick numbers
scatter3D(x,y,z, bty="g", colkey = F, main="bty=g", ticktype="detailed")


# • f – full box.
# • b – only the back panels are visible (default).
# • b2 – black panels with visible grid lines.
# • n – no box is drawn, same as setting box=FALSE.
# • g – grey background and white grinds lines.
# • bl – black background.
# • bl2 – black background with white grid lines.
# • u – user will specify the arguments col.axis, col.panel, lwd.panel, col.grid, lwd.grid manually.

# User defined
scatter3D(x,y,z, pch=18, bty="u", colkey=F, main="bty=u", col.panel="royalblue", expand=0.4, col.grid="linen")

################## 3D plots with threejs ################
install.packages("threejs")
install.packages("dplyr")

library(dplyr)
library(threejs)

############### EXERCISE 2 ###########
scatterplot3js(x,y,z, color = rainbow(length(z)))

# changing the style of the data point
scatterplot3js(x,y,z, color = heat.colors(length(z)))

# specifying color for each type of iris
N = length(levels(iris$Species))
scatterplot3js(x,y,z, size = 0.6, color = rainbow(N)[iris$Species])

# change the style of data point
scatterplot3js(x,y,z, pch="o", color = rainbow(length(z)))

# complex graphs: network analysis or population analysis using the maps package
data("ego")
graphjs(ego, bg="black")

# using globejs() to plot population in the 500 largest cities in the world
install.packages("maps")
data(world.cities, package = "maps")
cities <- world.cities[order(world.cities$pop, decreasing = T)[1:500],]
value <- 100 * cities$pop / max(cities$pop)

globejs(bg = "white",
        lat = cities$lat,
        long = cities$long,
        value = value,
        rorationlat = -0.34,
        rotationlong = -0.38, 
        fov = 30)

# 3D plots with rgl
install.packages("rgl")
library(rgl)

############## EXERCISE 3 ##############
plot3d(x,y,z)
# add color to the data points and increase size
plot3d(x,y,z,
       size = 7,
       col = as.numeric(iris$Species))

# changing the style of data point using type parameter
plot3d(x,y,z, type = "s", size = 2, col = as.numeric(iris$Species))

# add labels
plot3d(x,y,z, type = "s", size = 2, col = as.numeric(iris$Species),
       xlab = "Sepal Length",
       ylab = "Sepal Width",
       zlab = "Petal Length")

# creating an animated 3d scatterplot chart
# static chart
plot3d(x,y,z, col = as.numeric(iris$Species), type = "s", radius = .2)

# indicate the axis and the rotation velocity
play3d(spin3d(axis = c(0,1,0), rpm = 20), duration = 10)

# to save a gif file, use movie3d() function
# first install the magick package to run movie3d
install.packages("magick")
library(magick)

movie3d(movie = "3DAnimatedScatterPlot",
        spin3d(axis = c(0,0,1), rpm = 7),
        duration = 5,
        dir = getwd(),
        type = "gif",
        clean = TRUE)

#### plots using plotly
install.packages("plotly")
library(plotly)

############## QUESTION 4 #############
# scatter plot
plot_ly(iris, x=~x, y=~y, z=~z)

# you can also pass the names of the variables
plot_ly(iris,
        x=~Sepal.Length,
        y=~Sepal.Width,
        z=~Petal.Length)

# add colour
plot_ly(iris, x=~x, y=~y, z=~z, color = ~Species)

p <- plot_ly(iris, x=~x, y=~y, z=~z)
p <- add_markers(p, color = ~Species)
p

# or use pipes
p <- plot_ly(iris, x=~x, y=~y, z=~z) %>%
  add_markers(p, color = ~Species)
p

# add labels to the plot
variables <-names(iris)
layout(p, scene = list(xaxis = list(title = variables[1]),
                       yaxis = list(title = variables[2]),
                       zaxis = list(title = variables[3])))
