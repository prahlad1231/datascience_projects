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


# Plotting the bar graph of the above columns for top 5 categories
colnames(col_array)
labels <- c("Experience Level", "Employment Type", "Job Title", "Salary Currency", "Employee Residence", "Company Location", "Company Size")
for (i in 1:length(sorted_frequency_of_unique_values)) {
  barplot(head(sorted_frequency_of_unique_values[[i]]), main=labels[i], xlab="Category", ylab="Count")
}

# finding the trends in the average salaries in each year
library(dplyr)
library(ggplot2)
# first, grouping the data by work_year and then finding the average salary
average_annual_salary <- salary_df %>% 
  group_by(salary_df$work_year) %>% 
  summarise(average_salary=mean(salary_in_usd))
print(average_annual_salary)

# plotting a trend line
ggplot(average_annual_salary, aes(x=sort(unique_work_years), y=average_salary)) +
  geom_line() +
  labs(title = "Trends of Average Annual Salary", x="Work Year", y="Average Salary")

# getting the top 5 job titles and their salaries
top_five_jobs <- salary_df %>% 
  group_by(job_title) %>% 
  summarise(average_salary = mean(salary)) %>% 
  arrange(desc(average_salary)) %>% 
  head()

print(top_five_jobs)

# creating a color palette
colors <- c("red", "blue", "green", "orange", "purple", "yellow")
ggplot(top_five_jobs, aes(x=job_title, y=average_salary, fill=job_title)) +
  geom_col() +
  scale_fill_manual(values = colors) + 
  labs(title = "Top Five Jobs Titles With Salaries", x = "Job Title", y = "Salary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

top_five_job_titles <- head(sort(table(salary_df$job_title), decreasing=T), 5)
top_five_job_titles


# salary based on currency
salary_based_on_currency <- salary_df %>% 
  group_by(salary_currency) %>% 
  summarise(average_salary = mean(salary)) %>% 
  arrange(desc(average_salary)) %>% 
  head()

print(salary_based_on_currency)

ggplot(salary_based_on_currency, aes(x=salary_currency, y=average_salary, fill=salary_currency)) +
  geom_col() +
  scale_fill_manual(values = colors) + 
  labs(title = "Salary vs. Currency", x="Salary", y = "Currency")


# trends in the salaries based on company size
salary_based_on_company_size <- salary_df %>% 
  group_by(company_size) %>% 
  summarise(average_salary = mean(salary))
salary_based_on_company_size

ggplot(salary_based_on_company_size, aes(x=company_size, y=average_salary, fill=company_size)) +
  geom_col() +
  labs(title = "Average Salary based on Company Size", x="Company Size", y="Average Salary")


# distribution of salary by experience level
salary_by_experience <- salary_df %>% 
  group_by(experience_level) %>% 
  summarise(average_salary = mean(salary))
salary_by_experience

ggplot(salary_by_experience, aes(x=experience_level, y=average_salary, fill=experience_level)) +
  geom_col() +
  labs(title = "Average Salary based on Experience", x="Experience", y="Average Salary")

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
  summarise(average_salary = mean(salary))
print(salary_by_employment_type)

ggplot(salary_by_employment_type, aes(x=employment_type, y=average_salary, fill=employment_type)) +
  geom_col() +
  labs(title = "Average Salary based on Employment Type", x="Employment Type", y="Average Salary")
