library(tidyverse)
# Clarifications on the packages we'll be using today:
### {base} and {stats} are auto-loaded
### {ggplot2} is loaded with {tidyverse}
### {janitor} we only use once so will call it by using "package::function" approach 
# Read in the data from course website 
df <- read.csv("https://daviddliebowitz.github.io/EDUC641_22F/data/life_expectancy.csv") 
### Check out the dataset at https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who
head(df)
# We're interested in the relationship between two variables: 
### total_expenditure: government expenditure on health (% of total expenditure)
### life_expectancy: life Expectancy in age
# Prepare the dataset:
df <- df %>%  
  janitor::clean_names() %>% 
  select(total_expenditure, life_expectancy) %>% 
  drop_na() %>% 
  mutate(life_expectancy = round(life_expectancy, digits = 0))
# Summarize each variable
summary(df$total_expenditure)
summary(df$life_expectancy)

# Visualizing the relationship by scatterplots (two ways)

plot(df$total_expenditure, df$life_expectancy,
     xlab = "Expenditure on Health (% of Total)",
     ylab = "Life Expectancy (age)")

ggplot(df, aes(total_expenditure, life_expectancy)) +
  geom_point(color = "royalblue", alpha = 0.6) +
  labs(x = "Expenditure on Health (% of Total)",
       y = "Life Expectancy (age)")

# Best fit lines (two ways)
plot(df$total_expenditure, df$life_expectancy,
     xlab = "Expenditure on Health (% of Total)",
     ylab = "Life Expectancy (age)",
     ylim = c(0, 100))
abline(lm(df$life_expectancy ~ df$total_expenditure),
       col = "brown")

ggplot(df, aes(total_expenditure, life_expectancy)) +
  geom_point(color = "royalblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "brown") +
  labs(x = "Expenditure on Health (% of Total)",
       y = "Life Expectancy (age)") +
  ylim(0, 100)

# Fit a bivariate regression model to investigate this relationship
lm(life_expectancy ~ total_expenditure, df)

# Store the result of this bivariate regression in a list we call "model"
model <- lm(life_expectancy ~ total_expenditure, df)

# Model summary
summary(model)

# Extract parameters
model$coefficients
model$coefficients[2]

modelsummary(model)
modelsummary(model,
             stars = TRUE,
             gof_omit = "Adj.|Log.Lik.|F|AIC|BIC|RMSE",
             coef_rename = c('total_expenditure' = 'Expenditure on Health'))

|||||||||||||



