# a the five visual elements on this graph that are not ideal are:
#   1. the values are hovering on the top of the graph, which makes the it difficult to read
#   2. the goal line is not at 90
#   3. position of legend
#   4. There are too many lines in the graph
#   5. proper label could be given in the x-axis

# changes that could be made to graph
#   1. remove the values from top of the graph
#   2. make the goal line on 90
#   3. legend could be shown on top right corner
#   4. Remove the unnecessary lines from the graph
#   5. Month name could be the label on x-axis and year could be the title on x-axis


#loading libraries
library(tidyverse)
library(tidyr)
library(ggplot2)
library(plotly)

#extracting the data from graph

# Load the data
df <- data.frame(
  Month = c("January","Feburary", "March","April", "May", "June", "July", "August", "September", "October", "November", "December"),
  Direct_Sales = c(88.2, 76.3, 47.8, 76.1, 71.4, 58.6, 79.9, 69.4, 53.9, 80.8, 74.3, 46.4),
  Indirect_Sales = c(82.2, 71.4, 88.7, 81.0, 88.4, 120.2, 83.5, 73.8, 98.0, 85.1, 94.4, 64.8)
)

# Reshape the data into long format
library(tidyr)
df_long <- pivot_longer(df, cols = -Month, names_to = "Sales_Type", values_to = "Sales_Value")

# Create a ggplot bar chart showing both Direct Sales and Indirect Sales side by side
bar_graph <- ggplot(df_long, aes(x = factor(Month, levels = unique(Month)), y = Sales_Value, fill = Sales_Type)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_hline(yintercept = 90, linetype = "dotted", color = "red") +
  labs(
    title = "Sales required to close the deal",
    x = "Months from Year 2019",
    y = "Total Sales"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, max(df$Direct_Sales, df$Indirect_Sales), by = 20))+
  scale_fill_manual(values = c("Direct_Sales" = "blue", "Indirect_Sales" = "skyblue")) 

interactive_bargraph <- ggplotly(bar_graph)
interactive_bargraph


