
library(tidyverse)
# Clarification on the packages we'll be using today:
# {base} and {stats} are auto-loaded
# {ggplot2} is loaded with {tidyverse}
# {janitor} we only use once so will call it by using "package::function" approach 

# Read in the data from course website 
df <- read.csv("https://daviddliebowitz.github.io/EDUC641_22F/data/life_expectancy.csv") 
### Check out the dataset at https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who

# We're interested in the relationship between two variables: 
### total_expenditure: government expenditure on health (% of total expenditure)
### life_expectancy: life Expectancy in age

head(df)
summary(df$total_expenditure)
summary(df$life_expectancy)

# Prepare the dataset:

### uniform the variable names
df1 <- janitor::clean_names(df)
### select the two variables
df2 <- select(df1, total_expenditure, life_expectancy)
### use listwise deletion to deal with missing values
df3 <- drop_na(df2)
### recode the variable life_expectancy to be rounded ages
df4 <- mutate(df3, life_expectancy = round(life_expectancy, digits = 0))

### alternatively, using piping ("feeding" the output to next function)
df4 <- df %>%  
  janitor::clean_names() %>% 
  select(total_expenditure, life_expectancy) %>% 
  drop_na() %>% 
  mutate(life_expectancy = round(life_expectancy, digits = 0))

# Visualizing the relationship by scatterplots

ggplot(df4, aes(total_expenditure, life_expectancy)) +
ggplot2::geom_point() +
  geom_smooth()

base::plot(df4$total_expenditure, df4$life_expectancy)

# we fit a bivariate regression to investigate this relationship

stats::lm(life_expectancy ~ total_expenditure, df)

# we store the result of this bivariate regression in a list we call "model"
model <- lm(life_expectancy ~ total_expenditure, df)

# we look at the 
model$coefficients
