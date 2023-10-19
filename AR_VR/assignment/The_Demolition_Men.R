#Unit:RegressionModelling
#Assignment 
#cleaning the environment
rm(list=ls()) 

#installing the packages
install.packages("tidyverse")
install.packages("rvest")
library(rvest)
library(tidyverse)
library(stringr)
library(caret)
library(dplyr)
library(glmnet)
library(explore)
install.packages("explore")
require(tcltk)
library(ggplot2)
msgBox <- tkmessageBox(title = "Information about the code",
                       message = "Lets Load and Explore the DataSet we have selected!", icon = "info")
#Loading the data in R 
df <- read.csv("ds_salaries.csv")
#df_1 <- read.csv("ds_salaries.csv")
typeof(df)
head(df)
class(df)
#Lets Explore few rows of the dataset
head(df)
#lets check shape of the dataset
dim(df)

#we can see we have 3755 Rows, and 11 columns.
summary(df)
#lets check the NA values
colSums(is.na(df))
#printing the number of unique values for each categorical variable
categorical <- c("experience_level", "employment_type", "job_title", "employee_residence", "company_location", "company_size")
for (i in categorical) {
  cat(paste("Unique values in", i, ":", paste(unique(df[[i]]), collapse = ", "), "\n\n"))
}
cat("\nNumber of unique values in Categorical variables:\n")
#counting and printing the number of unique values for each categorical variable
unique_counts <- sapply(df[categorical], function(x) length(unique(x)))
cat(paste(names(unique_counts), ":", unique_counts, "\n"))
#lets see the correlation
# Assuming ds_salaries is your data frame
# First, load the corrplot package if you haven't already
# Sample data with categorical variables
#data <- data.frame(
  #experience_level = c("EN", "MI", "SE", "EX", "SE", "MI"),
 # employment_type = c("FT", "CT", "FL", "PT", "FT", "CT"),
  #company_size = c("L", "S", "M", "L", "S", "M")
#)
#converting experience_level to numeric
df$experience_level_numeric <- as.numeric(factor(df$experience_level, levels = c("EN", "MI", "SE", "EX")))
#converting employment_type to numeric
df$employment_type_numeric <- as.numeric(factor(df$employment_type, levels = c("FT", "CT", "FL", "PT")))
#converting company_size to numeric
df$company_size_numeric <- as.numeric(factor(df$company_size, levels = c("L","M","S")))

# Create a mapping of the original category labels to their numeric labels
company_size_mapping <- data.frame(
  Category_Label = c("S", "M", "L"),
  Numeric_Label = as.numeric(factor(c("S", "M", "L"), levels = c("S", "M", "L")))
)
# Loop through each category and print the mapping
for (category in c("S", "M", "L")) {
  cat("Numeric label of '", category, "' in company_size: ", company_size_mapping$Numeric_Label[company_size_mapping$Category_Label == category], "\n")
}
# Create a mapping of the original category labels to their numeric labels for experience_level
experience_level_mapping <- data.frame(
  Category_Label = c("EN", "MI", "SE", "EX"),
  Numeric_Label = as.numeric(factor(c("EN", "MI", "SE", "EX"), levels = c("EN", "MI", "SE", "EX"))
  )
)
#Print the mapping for experience_level
for (category in c("EN", "MI", "SE", "EX")) {
  cat("Numeric label of '", category, "' in experience_level: ", experience_level_mapping$Numeric_Label[experience_level_mapping$Category_Label == category], "\n")
}
# Create a mapping of the original category labels to their numeric labels for employment_type
employment_type_mapping <- data.frame(
  Category_Label = c("FT", "CT", "FL", "PT"),
  Numeric_Label = as.numeric(factor(c("FT", "CT", "FL", "PT"), levels = c("FT", "CT", "FL", "PT"))
  )
)
# Print the mapping for employment_type
for (category in c("FT", "CT", "FL", "PT")) {
  cat("Numeric label of '", category, "' in employment_type: ", employment_type_mapping$Numeric_Label[employment_type_mapping$Category_Label == category], "\n")
}

#viewing the resulting data frame
head(df)
colnames(df)
# Drop columns "column1" and "column2"
df_2 <- df %>%
  select(-company_size, -employment_type,-experience_level,-job_title,-employee_residence,-company_location,-salary_currency)
head(df_2)
install.packages("corrplot")

library(corrplot)

# Create a correlation matrix (example data)
correlation_matrix <- cor(df_2)

# Create a correlation plot with values
corrplot(correlation_matrix, method = "number")
#DataExplorer::create_report(df)
dim(df)
dim(df_2)
#Summary
#The DataSet contains 3755 rows and 7 columns. Initially we had 11 columns but as there were alot of 
#categorical variables  in job_title, employee_residence and company_location dropped them. and we converted experience_level
#employment_type, company_size
#unique(df$experience_level_numeric)
'Experience Level:
"EN" (Entry-level / Junior)-1
"MI" (Mid-level / Intermediate)-2
"SE" (Senior-level / Expert)-3
"EX" (Executive-level / Director)-4

Employment Type:
"FT" (Full-time)-1
"CT" (Contract)-2
"FL" (Freelance)-3
"PT" (Part-time)-4

Company Size:
"L" (More than 250 employees - Large)-3
"M" (50 to 250 employees - Medium)-2
"S" (Less than 50 employees - Small)-1'
# Renaming values in the "experience_level" column using direct assignment
#df_2$experience_level[df_2$experience_level == "EN"] <- "Entry-Level/Junior"
#df_2$experience_level[df_2$experience_level == "MI"] <- "Mid-level/Intermediate"
#df_2$experience_level[df_2$experience_level == "SE"] <- "Senior-level/Expert"
#df_2$experience_level[df_2$experience_level == "EX"] <- "Executive-level/Director"
# Create a countplot
ggplot(data = df, aes(x = experience_level)) +
  geom_bar() +
  labs(title = 'Experience Levels of Data Science Jobs',
       x = 'Experience Level',
       y = 'Count') +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
  )

# Reorder "job_title" based on count
df$job_title <- factor(df$job_title, levels = names(sort(table(df$job_title), decreasing = TRUE)))

# Filter the data to keep only the top 10 categories
df <- df[df$job_title %in% levels(df$job_title)[1:10], ]

# Create the bar chart
ggplot(df, aes(x = job_title)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = 'count', vjust = -0.5, size = 5, hjust = 1) + # Print count labels
  labs(title = 'Top 10 Job Titles in Data Science',
       x = 'Job Titles',
       y = 'Count') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 15, face = 'bold'),
        axis.title.y = element_text(size = 15, face = 'bold'),
        axis.text.y = element_text(size = 12)) + # Adjust label size
  coord_flip()


# Rename values in the "employment_type" column using direct assignment
df$employment_type[df$employment_type == "PT"] <- "Part-time"
df$employment_type[df$employment_type == "FT"] <- "Full-time"
df$employment_type[df$employment_type == "CT"] <- "Contract"
df$employment_type[df$employment_type == "FL"] <- "Freelance"



# Load the ggplot2 package
library(ggplot2)

# Assuming counts_df is your data frame

# Load the ggplot2 package if not already loaded
library(ggplot2)

# Create a bar plot with labels
p <- ggplot(df, aes(x = employment_type, y = count, label = count)) +
  geom_col(fill = "#0072B2", width = 0.6) + # Custom color and width
  geom_text(vjust = -0.5, size = 6, color = "black", fontface = "bold") + # Adjust label size and style
  labs(title = 'Employment Type of Data Science Jobs',
       x = 'Employment Type',
       y = 'Count') +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Adjust title size and style
    axis.title.x = element_text(size = 16, face = "bold"), # Adjust x-axis label size and style
    axis.title.y = element_text(size = 16, face = "bold"), # Adjust y-axis label size and style
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1), # Rotate x-axis labels for readability
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_blank(), # Remove border around the plot
    panel.background = element_blank() # Remove background color
  )

print(p)





# Assuming df_1 is your data frame

# Load the dplyr package if not already loaded
library(dplyr)

# Use the mutate function to replace values in the company_size column
df_1 <- df_1 %>%
  mutate(company_size = case_when(
    company_size == "S" ~ "Small(<50)",
    company_size == "M" ~ "Medium(50-250)",
    company_size == "L" ~ "Large(>250)",
    TRUE ~ company_size
  ))
#Company Size
ggplot(df_1, aes(x = company_size)) +
  geom_bar() +
  labs(title = 'Company Size', x = 'Company Size', y = 'Count') +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  geom_text(stat = 'count', aes(label = after_stat(count), vjust = -0.5), size = 4) +
  coord_flip()
# Create the countplot
ggplot(df, aes(x = work_year)) +
  geom_bar() +
  labs(title = "Work Year", x = "Work Year", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  geom_text(stat = 'count', aes(label = after_stat(count), vjust = -0.5), size = 6)


## Create the boxplot
# Assuming ds_salaries is your data frame

# Load the ggplot2 package if not already loaded
library(ggplot2)

# Create the boxplot and format y-axis labels as integers
ggplot(df, aes(y = salary_in_usd)) +
  geom_boxplot() +
  labs(title = "Salary Distribution in USD", y = "Salary in 1000X USD") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, accuracy = 1))


# Create the distribution plot
ggplot(df, aes(x = salary_in_usd)) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution plot of Data Science Salaries in USD", x = "Salary in USD") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, accuracy = 1))



# Load the ggplot2 package if not already loaded
# Load the ggplot2 package if not already loaded
library(ggplot2)



# Define custom labels for "experience_level"
custom_labels <- c("Entry-level / Junior", "Mid-level / Intermediate", "Senior-level / Expert", "Executive-level / Director")

# Set "experience_level" as a factor with custom labels
count_data$experience_level <- factor(count_data$experience_level, levels = unique(count_data$experience_level), labels = custom_labels)

# Create the countplot with hue
ggplot(count_data, aes(x = employment_type, y = count, fill = experience_level)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = 'Employment Type by Experience Level', x = 'Employment Type', y = 'Count') +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = 'bold')) +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 6)


# Assuming ds_salaries is your data frame

# Calculate the frequencies of each job title
title_frequencies <- table(df$job_title)

# Get the top 3 most common job titles
top_titles <- names(sort(title_frequencies, decreasing = TRUE))[1:3]

# Display the top titles
top_titles
# Load the required libraries if not already loaded
library(ggplot2)
library(dplyr)

# Get the top 3 job titles by experience level
top_titles <- df %>%
  group_by(job_title, experience_level) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  top_n(5)

# Create the plot
ggplot(top_titles, aes(x = fct_inorder(job_title), y = Count, fill = experience_level)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = 'Top 3 Job Titles by Experience Level',
       x = 'Job Title',
       y = 'Count') +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.title.y = element_text(size = 12, face = 'bold')) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  guides(fill = guide_legend(title = 'Experience Level'))




# Load the required libraries if not already loaded
library(ggplot2)
library(tidyr)
library(dplyr)

# Create the cross-tabulation
size_level_pivot <- df %>%
  group_by(company_size, experience_level) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = experience_level, values_from = Count, values_fill = 0)

# Reshape the data for plotting
size_level_pivot <- size_level_pivot %>%
  gather(key = "experience_level", value = "Count", -company_size)

# Create the bar plot
ggplot(size_level_pivot, aes(x = company_size, y = Count, fill = experience_level)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = 'Company Size by Experience Level',
       x = 'Company Size',
       y = 'Count') +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.title.y = element_text(size = 12, face = 'bold')) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  guides(fill = guide_legend(title = 'Experience Level'))



# Load the required libraries if not already loaded
library(dplyr)

# Create the pivot table and sort it by 'salary_in_usd' in descending order
title_sal_pivot <- df %>%
  group_by(job_title) %>%
  summarize(max_salary_in_usd = max(salary_in_usd)) %>%
  arrange(desc(max_salary_in_usd))

# Display the resulting data frame
title_sal_pivot


# Load the required libraries if not already loaded
library(ggplot2)

# Filter the top 10 job titles with the highest salary
top_10_titles <- head(title_sal_pivot, 10)

# Create the bar plot
ggplot(top_10_titles, aes(x = fct_reorder(job_title, max_salary_in_usd), y = max_salary_in_usd)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(title = 'Top 10 Job Titles with Highest Salary',
       x = 'Job Title',
       y = 'Salary in USD') +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12)) +
  geom_text(aes(label = max_salary_in_usd), vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = 'K'))  # Format salary as 1000X
