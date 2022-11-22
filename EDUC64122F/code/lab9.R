# Clarifications on the packages we'll be using today:
### what's new: install {modelsummary} then load it 
### other packages are the same with lab 8
library(tidyverse)
library(modelsummary)

# Read in the data from course website 
df <- read.csv("https://daviddliebowitz.github.io/EDUC641_22F/data/life_expectancy.csv") 
### Check https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who for data details

# Produce a summary statistics table using datasummary_skim() from {modelsummary}
### but the following line of code won't work well, why?
datasummary_skim(df)

### Because datasummary_skim() summarizes continuous and categorical variables differently
### And it doesn't allow users to do two types together in a single table
### So first thing first, look at the type of variables
str(df)
### Then select the variables you'll be using and recode them as needed
#### status: developed or developing country 
#### total_expenditure: government expenditure on health (% of total expenditure)
#### life_expectancy: life Expectancy in age
df_lab9 <- df %>%  
  janitor::clean_names() %>% 
  select(status, total_expenditure, life_expectancy) %>% 
  mutate(life_expectancy = round(life_expectancy, digits = 0)) 
### Summarizing continuous variables
datasummary_skim(df_lab9)
### Summarizing categorical variables
datasummary_skim(df_lab9, type="categorical")

# Create a regression table
m1 <- lm(life_expectancy ~ total_expenditure, df_lab9)
modelsummary(m1)

### Omit the statistics you don't need
modelsummary(m1,
             gof_omit = "Adj.|Log.Lik.|F|AIC|BIC|RMSE")

### Add legend for alpha-threshold at the bottom of the table,
### and the starts after coefficients to flag the significance level of p-value
modelsummary(m1,
             stars = TRUE,
             gof_omit = "Adj.|Log.Lik.|F|AIC|BIC|RMSE")

### Even fancier
modelsummary(list("Full sample" = m1),
             stars = TRUE,
             gof_omit = "Adj.&Log.Lik.|F|AIC|BIC|RMSE",
             coef_rename = c('total_expenditure' = 'Expenditure on Health'))


# You don't need the following code in assignment 4 but just for your curiosity
### Does the relationship between expectancy and expenditure differ for developed vs developing countries?
ggplot(df_lab9, aes(total_expenditure, life_expectancy)) +
  geom_point(color = "royalblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "brown") +
  labs(x = "Expenditure on Health (% of Total)",
       y = "Life Expectancy (age)") +
  ylim(0, 100) +
  facet_wrap(~status)

### Fit two more models
df_lab9_dvlped <- df_lab9 %>% 
  filter(status == "Developed")
df_lab9_dvlping <- df_lab9 %>% 
  filter(status == "Developing")

m2 <- lm(life_expectancy ~ total_expenditure, df_lab9_dvlped)
m3 <- lm(life_expectancy ~ total_expenditure, df_lab9_dvlping)

### Put three models side-by-side 
modelsummary(list("Full sample" = m1,
                  "Developed countries" = m2,
                  "Developing countries" = m3), 
             stars = TRUE,
             gof_omit = "Adj.|Log.Lik.|F|AIC|BIC|RMSE",
             coef_rename = c('total_expenditure' = 'Expenditure on Health'))

