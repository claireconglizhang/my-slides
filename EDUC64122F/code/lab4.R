library(tidyverse)

# We've learned all these, choose one for yourself:
df <- read.csv("https://daviddliebowitz.github.io/EDUC641_22F/data/deathpenalty.csv")
df <- read.csv("./data/deathpenalty.csv")
df <- read.csv("data/deathpenalty.csv")
# one-time usage of package {here} and its function 'here()'
df <- read.csv(here::here("data", "deathpenalty.csv")) 

# New way to import data: one-time usage of package {rio} and its function 'import()':
df_foo <- rio::import(here::here("data", "deathpenalty.csv"))

foo <- mutate(df, v = rvictim + rdefend)

# Rearrange the data by a variable (which variable?), then display the first ten rows:
head(arrange(df, rdefend), 10)

# Recode the categorical variables to have meaningful label for each level
df$rvictim <- factor(df$rvictim,
                     levels = c(1,2), labels=c("Black", "White"))
df$rdefend <- factor(df$rdefend,
                     levels = c(1,2), labels=c("Black", "White"))
df$deathpen <- factor(df$deathpen, 
                      levels = c(0,1), labels = c("No", "Yes"))

# Relationship between 'rvictim' and 'deathpen' in contingency tables

## Frequency in cell, choose one for yourself:
table1 <- table(df$deathpen, df$rvictim)
table1 <- xtabs(~ deathpen + rvictim, df)

## Percentage in cell, do both:
table2 <- round(prop.table(table1), 2)
table3 <- round(prop.table(table1, margin = 2), 2)

## Export your tables out of RStudio:
write.csv(table1, file = "./table/table1.csv", row.names = FALSE)
write.csv(table2, file = "./table/table2.csv", row.names = FALSE)
write.csv(table3, file = "./table/table3.csv", row.names = FALSE)

# Relationship between 'rvictim' and 'deathpen' in grouped bar charts

## Visualizing frequencies:
plot1 <- ggplot(df, aes(rvictim, fill = deathpen)) + 
  geom_bar(position = "dodge") + 
  xlab("Race of Victim") +
  ylab("Count") +
  ggtitle("Relationship between 'rvictim' and 'deathpen' 1") + 
  scale_fill_discrete(name = "Death Sentence") +
  theme_bw()

## Visualizing proportions:
plot2 <- ggplot(df, aes(rvictim, fill = deathpen)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.6) + 
  xlab("Race of Victim") +
  ylab("Proportion") +
  ggtitle("Relationship between 'rvictim' and 'deathpen' 2") + 
  ylim(0,0.75) +
  scale_fill_discrete(name = "Death Sentence") +
  theme_bw()

## Export your plots out of RStudio:
ggsave(plot = plot1, "./plot/plot1.jpeg")
ggsave(plot = plot2, "./plot/plot2.jpeg")

# Perform Pearson's Chi-squared Test

## Using Base R function 'chisq.test()': 
chi <- chisq.test(df$deathpen, df$rvictim)

## Understand the results of the test:
chi$observed
chi$expected
chi$statistic
chi$p.value

# Subsetting data

df2 <- filter(df, rdefend == "Black")
head(arrange(df2, rdefend), 10)
table(df2$rdefend)

df3 <- filter(df, rdefend == "White")
head(arrange(df3, rdefend), 10)
table(df3$rdefend)

# Re-investigate the relationship between 'rvictim' and 'deathpen' in sub-sample

table(df2$deathpen, df2$rvictim)
round(prop.table(table(df2$deathpen, df2$rvictim), margin = 2), 2)

ggplot(df, aes(rvictim, fill = deathpen)) + 
  geom_bar(position = "dodge") + 
  xlab("Race of Victim") +
  ylab("Count") +
  ggtitle("Relationship between 'rvictim' and 'deathpen' when defendant is black") + 
  scale_fill_discrete(name = "Death Sentence") +
  theme_bw()

chi2 <- chisq.test(df2$deathpen, df2$rvictim)
chi2$observed
chi2$expected
chi2$statistic
chi2$p.value

