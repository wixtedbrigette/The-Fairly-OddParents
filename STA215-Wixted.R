## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
mean(data$Wishes_About)
mean(data$Duration_of_Wishes)
mean(data$Affected_by_Wish)
mean(data$Number_Affected)
sd(data$Wishes_About)
sd(data$Duration_of_Wishes)
sd(data$Affected_by_Wish)
sd(data$Number_Affected)
table(data$Wishes_About)
table(data$Duration_of_Wishes)
table(data$Affected_by_Wish)
table(data$Number_Affected)
describe(data$Wishes_About)
describe(data$Duration_of_Wishes)
describe(data$Affected_by_Wish)
describe(data$Number_Affected)
summary(data$Wishes_About)
summary(data$Duration_of_Wishes)
summary(data$Affected_by_Wish)
summary(data$Number_Affected)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Wishes_About, data$Affected_by_Wish)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
table(data$Wishes_About, data$Affected_by_Wish)
chisq.test(table(data$Wishes_About, data$Affected_by_Wish))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(Number_Affected ~ Affected_by_Wish, data = data)
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$Duration_of_Wishes, data$Number_Affected)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Number_Affected ~ Duration_of_Wishes, data = data)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
linear_plot <- plot(data$Duration_of_Wishes, data$Number_Affected)
print(linear_plot)
linear_relationship <- lm(Number_Affected ~ Duration_of_Wishes, data = data)
summary(linear_relationship)
plot(data$Duration_of_Wishes, data$Number_Affected)
abline(h=mean(data$Number_Affected))
abline(h=mean(data$Duration_of_Wishes))
abline(linear_relationship, col = "red")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Duration_of_Wishes, residuals(linear_relationship))
abline(h=mean(data$Duration_of_Wishes))
       