library(tidyverse)
library(modelsummary)


# Read in the data from course website 
ah01 <- read.csv("https://daviddliebowitz.github.io/EDUC641_22F/data/ah01.csv") 
ah02 <- read.csv("https://daviddliebowitz.github.io/EDUC641_22F/data/ah02.csv") 

# Produce a summary statistics table using datasummary_skim() from {modelsummary}
### not exactly what i want:
datasummary_skim(ah01)
datasummary_skim(ah02)

### a bit data management:
ah011 <- ah01 %>%  
  select(-X) %>% 
  mutate(id = as.character(id),
         mentor = factor(mentor, levels = c(1,0), labels = c("Yes", "No")),
         gpa_3 = factor(gpa_3, levels = c(1, 0), labels = c("Yes", "No"))) 

ah022 <- ah02 %>%  
  select(-X) %>% 
  mutate(id = as.character(id),
         gpa_3 = factor(gpa_3, levels = c(1, 0), labels = c("Yes", "No"))) 

### Summarizing variables
datasummary_skim(ah011)
datasummary_skim(ah011, type="categorical")
datasummary_skim(ah022)
datasummary_skim(ah022, type="categorical")

# Code for research question 1
ah011 %>% 
  ggplot(aes(mentor)) +
  geom_bar(aes(fill = gpa_3),
           alpha = 0.83,
           position = "dodge") +
  labs(x = "Whether had a mentor",
       y = "Number of students") +
  scale_fill_discrete(name = "GPA above 3") +
  theme_bw()

chisq.test(ah011$mentor, ah011$gpa_3)

# Code for research question 2
fooo <- mean(ah022$education)
ah022 %>% 
  ggplot(aes(education)) +
  geom_histogram(fill = "grey60") +
  geom_vline(xintercept = fooo, color = "blue") +
  geom_vline(xintercept = 14.7, color = "brown") +
  geom_text(aes(x = fooo, label = "Average education attainment of students who had a mentor", y = 20), angle = 90, vjust = 1.5) +
  geom_text(aes(x = 14.7, label = "Average education attainment of students who didn't have a mentor", y = 20), angle = 90, vjust = 1.5) +
  labs(x = "Education attainment of students who had a mentor",
       y = "Number of students") +
  theme_bw()

chisq.test(ah011$mentor, ah011$gpa_3)

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

