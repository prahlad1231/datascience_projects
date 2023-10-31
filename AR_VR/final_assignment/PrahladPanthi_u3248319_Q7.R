# a
# Seeing the trends in the dataset, we can conclude that the rate of cholesterol has increased for all the test groups 
# over the period of time from 2015 to 2019. Thus, we cannot use the product in the market.

# b
# Plot: Over a five-year period from 2015 to 2019, we observed a noteworthy trend in cholesterol rates across multiple 
# research centers (A-M). This analysis is crucial as it sheds light on the evolving health landscape and allows us 
# to make informed decisions.
# 
# Rising Action: In 2015, we began our journey, noting that the average cholesterol rates in all research centers were relatively stable, with some minor variations.
# As we moved to 2016, a subtle yet consistent rise in cholesterol levels became evident, hinting at a potential health concern.
# 
# Climax: The turning point arrived in 2017 when the cholesterol rates notably increased across all research centers. 
# This escalation was statistically significant and couldn't be overlooked.
# 
# Falling Action: The year 2018 continued the upward trajectory, with cholesterol rates continuing to climb, reinforcing the 
# importance of our analysis. By 2019, the trend was unmistakable, and cholesterol levels reached a point where action was imperative.
# 
# Ending: As there is upward growth in the cholesterol rates of the test groups, the vaccine is not suitable for rolling out 
# for the public. There must be some changes made to the vaccine in order to stabalize and improve the rates in the test groups.


# c
# 
# I choose the line chart to support my claim. We can clearly see the increasing trend in the cholesterol level 
# of test groups in various research centers over the period of 5 years i.e. from 2015 to 2019. This is certainly not 
# a positive outcome that we were looking for, which suggests us that the vaccine is not yet ready for the broad market.
# Overall, the line chart effectively conveys the increasing trend in cholesterol rates over time and enables easy 
# comparison across different research centers, aligning with my Big Idea of the analysis.



# loading the necessary libraries
library(ggplot2)
library(tidyr)

# creating the data frame from given table 
df <- data.frame(
  Research_centre = c("ALL", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
  '2015' = paste0(c(7.20, 2.50, 10.70, 9.20, 8.30, 7.90, 8.70, 3.70, 8.40, 5.10, 6.40, 9.20, 9.90, 5.60), "%"),
  '2016' = paste0(c(7.60, 3.00, 11.40, 9.70, 8.60, 8.20, 9.10, 4.00, 8.80, 5.60, 6.80, 9.70, 10.20, 6.00), "%"),
  '2017' = paste0(c(8.00, 3.40, 11.90, 10.20, 9.00, 8.50, 9.60, 4.30, 9.20, 6.00, 7.10, 10.10, 11.00, 6.20), "%"),
  '2018' = paste0(c(8.10, 3.70, 12.00, 10.40, 9.10, 10.10, 9.60, 4.50, 9.50, 6.20, 7.20, 10.20, 11.10, 6.30), "%"),
  '2019' = paste0(c(8.60, 4.40, 12.50, 10.90, 9.50, 11.30, 10.10, 5.00, 10.00, 6.70, 7.60, 10.50, 11.60, 6.70), "%")
)

head(df)

long_df <- gather(df, key = "Year", value = "Value", -Research_centre)
head(long_df)

########################################################################################################################
# Creating the bar plot
long_df$Value <- as.numeric(gsub("%", "", long_df$Value))  # Convert Value to numeric and remove "%" symbol

# ggplot(long_df, aes(x = Research_centre, y = Value, fill = Year)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "R&D data of Research Centres",
#        x = "Research Centres",
#        y = "Value") +
#   scale_fill_manual(values = c("X2015" = "red", "X2016" = "green", "X2017" = "blue", "X2018" = "yellow", "X2019" = "pink")) +
#   scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2), labels = paste0(seq(0, 14, by = 2), "%")) +
#   theme(legend.position = "right") + # making the legend to appear at the right hand side of the plot
#   labs(fill = "Years")  # Adding a title to the legend


########################################################################################################################




########################################################################################################################


# Create a custom rainbow color palette
rainbow_colors <- rainbow(length(unique(long_df$Research_centre)))

# Convert the "Value" variable to numeric (remove "%" and convert)
# long_df$Value <- as.numeric(gsub("%", "", long_df$Value))

# Create a named vector to map Research_centre to colors
color_mapping <- setNames(rainbow_colors, unique(long_df$Research_centre))

ggplot(long_df, aes(x = Year, y = Value, color = Research_centre, group = Research_centre)) +
  geom_line() +
  labs(title = "Research Centre Data",
       x = "Year",
       y = "Value") +
  scale_color_manual(values = color_mapping) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2), labels = paste0(seq(0, 14, by = 2), "%")) +
  scale_x_discrete(labels = function(x) gsub("X", "", x)) +  # Remove "X" prefix from x-axis labels
  theme(legend.position = "bottom", legend.title = element_blank())

