# Umesh - STEP 1: LIBRARIES & DATA LOADING

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# STEP 1: DATA LOADING
data <- read_csv("kaggle_survey_2020_responses.csv", show_col_types = FALSE)

cat("Dataset dimensions:\n")
print(dim(data))

cat("\nColumn names (first 20):\n")
print(head(colnames(data), 20))

# Umesh - STEP 2: DATA QUALITY ASSESSMENT

cat("\n=== DUPLICATE RECORDS CHECK ===\n")
cat("Total rows in original data:", nrow(data), "\n")
cat("Duplicate rows found:", sum(duplicated(data)), "\n")

cat("\n=== NULL VALUES SUMMARY ===\n")
null_summary <- data %>%
  summarise(
    total_rows = n(),
    null_gender = sum(is.na(Q2) | Q2 == ""),
    null_salary = sum(is.na(Q24) | Q24 == ""),
    null_both = sum((is.na(Q2) | Q2 == "") &
                    (is.na(Q24) | Q24 == ""))
  )

print(null_summary)

# Remove duplicates
data <- data %>% distinct()

cat("\nRows after removing duplicates:", nrow(data), "\n")

# Umesh - STEP 3: DATA CLEANING AND PREPROCESSING

data_clean <- data %>%
  mutate(
    gender = case_when(
      Q2 %in% c("Male", "Man") ~ "Male",
      Q2 %in% c("Female", "Woman") ~ "Female",
      TRUE ~ NA_character_
    ),
    salary_numeric = case_when(
      Q24 == "$0 ($USD)" ~ 0,
      Q24 == "1-2,999" ~ 1500,
      Q24 == "3,000-3,999" ~ 3500,
      Q24 == "4,000-4,999" ~ 4500,
      Q24 == "5,000-7,499" ~ 6250,
      Q24 == "7,500-9,999" ~ 8750,
      Q24 == "10,000-14,999" ~ 12500,
      Q24 == "15,000-19,999" ~ 17500,
      Q24 == "20,000-24,999" ~ 22500,
      Q24 == "25,000-29,999" ~ 27500,
      Q24 == "30,000-39,999" ~ 35000,
      Q24 == "40,000-49,999" ~ 45000,
      Q24 == "50,000-59,999" ~ 55000,
      Q24 == "60,000-69,999" ~ 65000,
      Q24 == "70,000-79,999" ~ 75000,
      Q24 == "80,000-89,999" ~ 85000,
      Q24 == "90,000-99,999" ~ 95000,
      Q24 == "100,000-124,999" ~ 112500,
      Q24 == "125,000-149,999" ~ 137500,
      Q24 == "150,000-199,999" ~ 175000,
      Q24 == "200,000-249,999" ~ 225000,
      Q24 == "250,000-299,999" ~ 275000,
      Q24 == "300,000-500,000" ~ 400000,
      Q24 == "> $500,000" ~ 500000,
      Q24 == "$100,000 or more ($USD)" ~ 100000,
      Q24 == "$10,000-$99,999" ~ 55000,
      Q24 == "$1-$99" ~ 50,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(salary_numeric)) %>%   # remove missing salary
  filter(salary_numeric > 0) %>%       # remove invalid zero values
  filter(!is.na(gender)) %>%           # remove missing gender
  mutate(gender_group = gender)        # rename column for later use

cat("\n=== DATA CLEANING SUMMARY ===\n")
cat("Original dataset rows:", nrow(data), "\n")
cat("Rows with valid salary and gender:", nrow(data_clean), "\n")
cat("Rows removed due to missing/invalid data:", nrow(data) - nrow(data_clean), "\n")

cat("Male respondents:", sum(data_clean$gender == "Male"), "\n")
cat("Female respondents:", sum(data_clean$gender == "Female"), "\n")

cat("Percentage Male:", round(sum(data_clean$gender == "Male")/nrow(data_clean)*100, 2), "%\n")
cat("Percentage Female:", round(sum(data_clean$gender == "Female")/nrow(data_clean)*100, 2), "%\n")