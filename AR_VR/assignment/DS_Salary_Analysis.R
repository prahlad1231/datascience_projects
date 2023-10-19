library(tidyverse)


salary_df <- read.csv("ds_salaries.csv")

# cheking if there are any null values in the dataframe
colSums(is.na(salary_df))

# Summary of the dataset
summary(salary_df)

colnames(salary_df)

# taking only the numeric columns
numeric_columns <- salary_df[, c(1, 5, 7, 9)]
head(numeric_columns)

summary(numeric_columns)

# taking the unique values of work_year
unique_work_years <- unique(salary_df$work_year)
unique_work_years

# finding the frequency of work years
frequency_work_year <- table(salary_df$work_year)
frequency_work_year

# plotting a pie chart showing the work year
color_palette <- rainbow(length(frequency_work_year))
pie(frequency_work_year, col = color_palette, main = "Distribution of Work Year")
legend("topright", as.character(sort(unique_work_years)), cex=1, fill = color_palette)


# showing same data in bar plot
barplot(frequency_work_year, xlab = "Year", ylab="Count", main = "Distribution of Work Year", col = color_palette)
legend("topleft", as.character(sort(unique_work_years)), fill = color_palette)

# selecting all the unique values from columns : experience_level, employment type, job title, and so on
colnames(salary_df)
col_array <- salary_df[, c(2, 3, 4, 6, 8, 10, 11)]
unique_values <- sapply(col_array, function(column) unique(column))
print(unique_values)


# finding the frequency of each values
frequency_of_unique_values <- sapply(col_array, function(column) table(column))
frequency_of_unique_values
# sorting the values based on their frequency (high to low)
sorted_frequency_of_unique_values <- lapply(frequency_of_unique_values, function(column) column[order(column, decreasing = TRUE)])
print(sorted_frequency_of_unique_values)


