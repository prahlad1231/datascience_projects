# Load the necessary packages
library(tidyverse)
library(tidyr)
library(ggplot2)
library(plotly)
library(aframer)

df <- read.csv("dataset_aframe.csv")
head(df)

# grouping the dataframe by target values i.e. 1 to 4
df <- df %>%
  group_by(target) %>%
  select(c(variable_1, variable_2, variable_3))
head(df)

# Define a color palette based on target values
color_palette <- c("red", "green", "blue", "yellow")

# Create a list of JSON objects
json_list <- lapply(1:nrow(df), function(i) {
  json_obj <- paste(
    '{"x": ', df[i, "variable_1"], ', "y": ', df[i, "variable_2"], ', "z": ', df[i, "variable_3"],
    ', "size": 1, "color": "', color_palette[as.numeric(df[i, "target"])], '"}', sep = ""
  )
  return(json_obj)
})

# Combine the JSON objects into a single string with line breaks
json_df <- paste(json_list, collapse = ",\n")
json_df <- paste("[\n", json_df, "\n]", sep = "")

# Remove line breaks and white space from the JSON string
cleaned_json_df <- gsub("\\s+", "", json_df)

writeLines(cleaned_json_df, "PrahladPanthi_u3248319_Q5_output.json")
