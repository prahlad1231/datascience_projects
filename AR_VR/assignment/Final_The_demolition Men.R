#Unit:11464 & 11524 AR/VR for Data Analysis and Communication
#Assignment_2 
#Team NAME:The Demolition Men
#Team Member:Birat Jung Thapa(u3245672)
#           :Prahlad Panthi(u3248319)
#           :Shital Ghimire(U3218501)
#cleaning the environment
rm(list=ls()) 
#installing the packages
install.packages("tidyverse")
install.packages("corrplot")
library(corrplot)
require(tcltk)
library(ggplot2)
msgBox <- tkmessageBox(title = "Information about the code",
                       message = "Lets Load and Explore the DataSet we have selected!", icon = "info")

#Loading the data in R 
df <- read.csv("ds_salaries.csv")
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
#we can see we do not have any NA values in any columns.

#printing the number of unique values for each categorical variable
categorical <- c("experience_level", "employment_type", "job_title", "employee_residence", "company_location", "company_size")
for (i in categorical) {
  cat(paste("Unique values in", i, ":", paste(unique(df[[i]]), collapse = ", "), "\n\n"))
}
cat("\nNumber of unique values in Categorical variables:\n")
#counting and printing the number of unique values for each categorical variable
unique_counts <- sapply(df[categorical], function(x) length(unique(x)))
cat(paste(names(unique_counts), ":", unique_counts, "\n"))

#we can see we have 4, 4, 93, 78, 72, and 3 categorical variables in exxperience level, exmploymenttype
#job_title, employee residence, company location and company size respectively.
#we will drop the columns with more than 4 categorical variables and we will label
#other columsn with numeric variable.

#converting experience_level to numeric
df$experience_level_numeric <- as.numeric(factor(df$experience_level, levels = c("EN", "MI", "SE", "EX")))

#converting employment_type to numeric
df$employment_type_numeric <- as.numeric(factor(df$employment_type, levels = c("FT", "CT", "FL", "PT")))

#converting company_size to numeric
df$company_size_numeric <- as.numeric(factor(df$company_size, levels = c("L","M","S")))

#creating a mapping of the original category labels to their numeric labels
company_size_mapping <- data.frame(
  Category_Label = c("S", "M", "L"),
  Numeric_Label = as.numeric(factor(c("S", "M", "L"), levels = c("S", "M", "L")))
)
#Loopping through each category and printing the mapping
for (category in c("S", "M", "L")) {
  cat("Numeric label of '", category, "' in company_size: ", company_size_mapping$Numeric_Label[company_size_mapping$Category_Label == category], "\n")
}
#creating a mapping of the original category labels to their numeric labels for experience_level
experience_level_mapping <- data.frame(
  Category_Label = c("EN", "MI", "SE", "EX"),
  Numeric_Label = as.numeric(factor(c("EN", "MI", "SE", "EX"), levels = c("EN", "MI", "SE", "EX"))
  )
)
#printing the mapping for experience_level
for (category in c("EN", "MI", "SE", "EX")) {
  cat("Numeric label of '", category, "' in experience_level: ", experience_level_mapping$Numeric_Label[experience_level_mapping$Category_Label == category], "\n")
}
#creating a mapping of the original category labels to their numeric labels for employment_type
employment_type_mapping <- data.frame(
  Category_Label = c("FT", "CT", "FL", "PT"),
  Numeric_Label = as.numeric(factor(c("FT", "CT", "FL", "PT"), levels = c("FT", "CT", "FL", "PT"))
  )
)
#printing the mapping for employment_type
for (category in c("FT", "CT", "FL", "PT")) {
  cat("Numeric label of '", category, "' in employment_type: ", employment_type_mapping$Numeric_Label[employment_type_mapping$Category_Label == category], "\n")
}

#viewing the resulting data frame
head(df)
colnames(df)

#droppiong columns "column1" and "column2"
df_2 <- df %>%
  select(-company_size, -employment_type,-experience_level,-job_title,-employee_residence,-company_location,-salary_currency)
head(df_2)

#creating a correlation matrix (example data)
correlation_matrix <- cor(df_2)

#creating a correlation plot with values
corrplot(correlation_matrix, method = "number")

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

# taking the unique values of work_year
unique_work_years <- unique(df$work_year)
unique_work_years

# finding the frequency of work years
frequency_work_year <- table(df$work_year)
frequency_work_year

color_palette <- rainbow(length(frequency_work_year))

# showing  data in bar plot
barplot(frequency_work_year, xlab = "Year", ylab="Count", main = "Distribution of Work Year", col = color_palette)
legend("topleft", as.character(sort(unique_work_years)), fill = color_palette)

col_array <- df[, c(2, 3, 4, 6, 8, 10, 11)]
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
  barplot(head(sorted_frequency_of_unique_values[[i]]), 
          main=labels[i], 
          xlab="Category", 
          ylab="Count", 
          col = rainbow(length(sorted_frequency_of_unique_values)))
}

library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Barplots with Shiny Dropdown"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category:", labels)
    ),
    mainPanel(
      plotOutput("barplot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$barplot <- renderPlot({
    i <- match(input$category, labels)
    colors_shiny <- rainbow(length(sorted_frequency_of_unique_values[[i]]))
    barplot(head(sorted_frequency_of_unique_values[[i]], 
                 ), col = colors_shiny)
    title(main = labels[i], xlab = "Category", ylab = "Count")
  })
}

# Run the Shiny app
shinyApp(ui=ui, server=server)

#Analyis-1
unique_work_years <- unique(df$work_year)

unique_work_years

average_annual_salary <- df %>% 
  group_by(df$work_year) %>% 
  summarise(average_salary=mean(salary_in_usd))
print(average_annual_salary)

#creating plot workyear vs Average salary in USD
ggplot(average_annual_salary, aes(x = sort(unique_work_years), y = average_salary)) +
  geom_line(size = 1.5, color = "red") + 
  labs(title = "Trends of Average Annual Salary", x = "Work Year", y = "Average Salary in USD") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "black"),  # Title style
    axis.title.x = element_text(size = 17, face = "bold", color = "black"),  # X-axis label style
    axis.title.y = element_text(size = 17, face = "bold", color = "black")  # Y-axis label style
  )

#Analysis-2
#lets explore if experience level is affecting the salary or Not?

# distribution of salary by experience level
salary_by_experience <- df %>% 
  group_by(experience_level) %>% 
  summarise(average_salary = mean(salary_in_usd))
salary_by_experience

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


#Analysis-3
# getting the top 5 job titles and their salaries
top_five_jobs <- df %>% 
  group_by(job_title) %>% 
  summarise(average_salary = mean(salary_in_usd)) %>% 
  arrange(desc(average_salary)) %>% 
  head(n=5)

print(top_five_jobs)

#defining a color palette
colors <- c("red", "blue", "green", "orange", "purple")

#creating the bar plot with formatted y-axis labels
ggplot(top_five_jobs, aes(x = job_title, y = average_salary, fill = job_title)) +
  geom_col() +
  scale_fill_manual(values = colors) + 
  labs(title = "Top Five Jobs Titles With Salaries", x = "Job Title", y = "Salary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels as integers

#Analyis_4
# trends in the salaries based on company size
salary_based_on_company_size <- df %>% 
  group_by(company_size) %>% 
  summarise(average_salary = mean(salary_in_usd))
salary_based_on_company_size
# Creating a color palette
colors <- c("#0072B2", "#56B4E9", "#009E73")

# Creatinfg the horizontal bar plot with custom aesthetics and formatted x-axis labels
ggplot(salary_based_on_company_size, aes(x = average_salary, y = company_size, fill = company_size)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = colors) +
  labs(title = "Average Salary Based on Company Size", x = "Average Salary in USD", y = "Company Size") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12, hjust = 1),
    legend.position = "none"
  ) +
  scale_x_continuous(labels = scales::comma)  # Format x-axis labels as integers


#Analysis 5
frequency_company_location <- table(df$company_location)
frequency_company_location
#Create a data frame with the top 10 countries
top_countries <- data.frame(
  Country = names(head(sort(frequency_company_location, decreasing = TRUE), 10)),
  Frequency = as.numeric(head(sort(frequency_company_location, decreasing = TRUE), 10))
)

# Create the bar plot
ggplot(top_countries, aes(x = reorder(Country, -Frequency), y = Frequency, fill = Country)) +
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

## Analysis 6
#in addition to this lets lets see the top 10 Job titles 
#reordering "job_title" based on count
df$job_title <- factor(df$job_title, levels = names(sort(table(df$job_title), decreasing = TRUE)))

#filtering the data to keep only the top 10 categories
df <- df[df$job_title %in% levels(df$job_title)[1:10], ]

#creating the bar chart
ggplot(df, aes(x = job_title)) +
  geom_bar(fill=rainbow(10)) +
  geom_text(aes(label = ..count..), stat = 'count', vjust = -0.5, size = 5, hjust = 1) + # Print count labels
  labs(title = 'Top 10 Job Titles in Data Science',
       x = 'Job Titles',
       y = 'Count') +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 17, face = 'bold'),
        axis.title.y = element_text(size = 17, face = 'bold'),
        axis.text.y = element_text(size = 11)) + # Adjust label size
  coord_flip()


