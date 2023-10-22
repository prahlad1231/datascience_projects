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
#color_palette <- rainbow(length(frequency_work_year))
#pie(frequency_work_year, col = color_palette, main = "Distribution of Work Year")
#legend("topright", as.character(sort(unique_work_years)), cex=0.7, fill = color_palette)

unique_work_years <- unique(salary_df$work_year)
frequency_work_year <- table(salary_df$work_year)

# Create a bar plot with all work years
color_palette <- rainbow(length(frequency_work_year))
barplot(frequency_work_year, 
        xlab = "Year", 
        ylab = "Count", 
        main = "Distribution of Work Year",
        col = color_palette)


# Add legend
legend("topleft", as.character(sort(unique_work_years)), fill = color_palette)

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


# Plotting the bar graph of the above columns for top 5 categories
colnames(col_array)
labels <- c("Experience Level", "Employment Type", "Job Title", "Salary Currency", "Employee Residence", "Company Location", "Company Size")
for (i in 1:length(sorted_frequency_of_unique_values)) {
  barplot(head(sorted_frequency_of_unique_values[[i]]), main=labels[i], xlab="Category", ylab="Count")
}

# Create a ggplot with bolder line, grey background, and bold labels
ggplot(average_annual_salary, aes(x = sort(unique_work_years), y = average_salary)) +
  geom_line(size = 1.5, color = "blue") +  # Adjust line size and color
  labs(title = "Trends of Average Annual Salary", x = "Work Year", y = "Average Salary in USD") +
  theme_minimal() +  # Set grey background
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black"),  # Title style
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),  # X-axis label style
    axis.title.y = element_text(size = 16, face = "bold", color = "black")  # Y-axis label style
  )
ggplot(top_five_jobs, aes(x = fct_reorder(job_title, -average_salary), y = average_salary, fill = job_title)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = colors) +
  labs(title = "Top Five Jobs Titles With Salaries", x = "Job Title", y = "Salary in USD") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black"),
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1, color = "black")
  ) +
  scale_y_continuous(labels = scales::comma)  # Format the y-axis labels with commas

# salary based on currency
salary_based_on_currency <- salary_df %>% 
  group_by(salary_currency) %>% 
  summarise(average_salary = mean(salary)) %>% 
  arrange(desc(average_salary)) %>% 
  head(n=5)

print(salary_based_on_currency)

ggplot(salary_based_on_currency, aes(x=salary_currency, y=average_salary, fill=salary_currency)) +
  geom_col() +
  scale_fill_manual(values = colors) + 
  labs(title = "Salary vs. Currency", x="Salary", y = "Currency")


library(ggplot2)

# Create a color palette for the plot
colors <- c("#0072B2", "#56B4E9", "#009E73")

# Create the horizontal bar plot with custom aesthetics
ggplot(salary_based_on_company_size, aes(x = average_salary, y = company_size, fill = company_size)) +
  geom_col(width = 0.7) +  # Adjust the width of columns
  scale_fill_manual(values = colors) +  # Set custom fill colors
  labs(title = "Average Salary Based on Company Size", x = "Average Salary in USD", y = "Company Size") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12, hjust = 1),  # Adjust the position of y-axis labels
    legend.position = "none"  # Remove legend
  )


ggplot(salary_by_experience, aes(x = fct_reorder(experience_level, -average_salary), y = average_salary, fill = experience_level)) +
  geom_col(width = 0.6, color = "black") +  # Adjust width and outline color
  labs(title = "Average Salary based on Experience Level", x = "Experience Level", y = "Average Salary") +
  theme_minimal() +  # Set a minimal theme for the plot
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "black"),  # Title style
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),  # X-axis label style
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),  # Y-axis label style
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = "black")  # X-axis text style
  )

# creating boxplot to see the distribution of salary based on experience level
ggplot(salary_df, aes(x=experience_level, y=scale(salary))) +
  geom_boxplot() +
  labs(title="Boxplot of Salary based on Experience Level", x='Experience Level', y='Salary') +
  theme_minimal()

# seeing the distribution of salary in usd and expeience level
ggplot(salary_df, aes(x=experience_level, y=salary_in_usd)) +
  geom_boxplot(fill="red") +
  labs(title="Distribution of Salary by Experience Level", x="Experience Level", y="Salary in USD") +
  theme_minimal()

salary_in_usd_histogram<-ggplot(salary_df, aes(x=salary_in_usd)) +
  geom_histogram(binwidth=10000, fill = "blue") +
  labs(title="Distribution of Salary in USD", x="Salary", y="Frequency")
salary_in_usd_histogram


# distribution of salary by employment type
salary_by_employment_type <- salary_df %>% 
  group_by(employment_type) %>% 
  summarise(average_salary = mean(salary_in_usd))
print(salary_by_employment_type)

ggplot(salary_by_employment_type, aes(x=employment_type, y=average_salary, fill=employment_type)) +
  geom_col() +
  labs(title = "Average Salary based on Employment Type", x="Employment Type", y="Average Salary")

ggplot(salary_df, aes(x=employment_type, y=salary_in_usd)) +
  geom_boxplot(fill="skyblue") +
  labs(title="Distribution of Salary by Employment Type", x="Employment Type", y="Salary")

unique_country <- unique(salary_df$company_location)
frequency_company_location <- table(salary_df$company_location)
frequency_company_location
library(ggplot2)

# Create a data frame with the top 10 countries
top_countries <- data.frame(
  Country = names(head(sort(frequency_company_location, decreasing = TRUE), 10)),
  Frequency = as.numeric(head(sort(frequency_company_location, decreasing = TRUE), 10))
)

# Create the bar plot
p <- ggplot(top_countries, aes(x = reorder(Country, -Frequency), y = Frequency, fill = Country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 4, color = "black") +  # Add text labels
  labs(title = "Top 10 Countries with the Most Data Science Jobs", x = "Country", y = "Number of Jobs") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

print(p)

