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