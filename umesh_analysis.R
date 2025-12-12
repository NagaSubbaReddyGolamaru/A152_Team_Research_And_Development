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