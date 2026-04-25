# Export all the files
library(tidyverse)
library(dplyr)
library(haven)
demo <- read_xpt(file.choose())
hba1c <- read_xpt(file.choose())
depress <- read_xpt(file.choose())

write.csv(demo, "C:/Users/Neha/OneDrive/Desktop/NHANES_Project_Diabetes/Demographics.csv", row.names = FALSE)
write.csv(hba1c, "C:/Users/Neha/OneDrive/Desktop/NHANES_Project_Diabetes/HbA1c_Lab.csv", row.names = FALSE)
write.csv(depress, "C:/Users/Neha/OneDrive/Desktop/NHANES_Project_Diabetes/Depression_Survey.csv", row.names = FALSE)

# Set the folder
setwd("C:/Users/Neha/OneDrive/Desktop/NHANES_Project_Diabetes")
data <- read.csv("clean_data_sql.csv")
head(data)
str(data)
summary(data)

# Check the variable count 
table(data$diabetes_status)
table(data$depressed_mood)
table(data$gender)

# Create one clean binary depression variable 
data$depression_binary <- ifelse(data$depressed_mood %in% c("Several days", "More than half the days", "Nearly every day"),
                                 1, 0)

table(data$depression_binary)

# Run the chi-square test

chisq.test(table(data$depression_binary, data$diabetes_status))

# Logistic Regression

mode1 <- glm(diabetes_status ~ depression_binary + age + gender,
             data = data,
             family = binomial)

summary(mode1)

exp(coef(mode1))

library(ggplot2)

ggplot(data, aes(x = depressed_mood, fill = factor(diabetes_status))) +
  geom_bar(position = "fill") +
  labs(y = "Proprtion",
       fill = "Diabetes",
       title = "Diabetes Prevalence by Depression Level")+
  theme_minimal()


ggplot(data, aes(x = depressed_mood, fill = factor(diabetes_status))) +
  geom_bar(position = "fill") +
  labs(
    title = "Diabetes Prevalence by Depression Level",
    x = "Depression Level",
    y = "Proportion",
    fill = "Diabetes Status"
  ) +
  scale_fill_manual(values = c("#2E86AB", "#E27D60"),
                    labels = c("No Diabetes", "Diabetes")) +
  theme_minimal()

ggsave("diabetes_vs_depression.png", width = 8, height = 5, dpi = 300)

# Diabetes vs Age

ggplot(data, aes(x = age, y = diabetes_status)) +
  geom_smooth(method = "loess", color = "#E27D60", se = TRUE) +
  labs(
    title = "Probability of Diabetes Across Age",
    x = "Age",
    y = "Probability of Diabetes"
  ) +
  theme_minimal()

ggsave("diabetes_vs_age.png", width = 8, height = 5, dpi = 300)


# Diabetes Vs Gender

ggplot(data, aes(x = gender, fill = factor(diabetes_status))) +
  geom_bar(position = "fill") +
  labs(
    title = "Diabetes Prevalence by Gender",
    x = "Gender",
    y = "Proportion",
    fill = "Diabetes Status"
  ) +
  scale_fill_manual(values = c("#2E86AB", "#E27D60"),
                    labels = c("No Diabetes", "Diabetes")) +
  theme_minimal()

ggsave("diabetes_vs_gender.png", width = 8, height = 5, dpi = 300)





