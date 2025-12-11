# STEP 7: STATISTICAL TESTING

male_salaries <- data_clean %>% 
  filter(gender == "Male") %>% 
  pull(salary_numeric)

female_salaries <- data_clean %>% 
  filter(gender == "Female") %>% 
  pull(salary_numeric)

cat("\n=== ASSUMPTION CHECKS ===\n")
cat("\nNormality Tests (Shapiro-Wilk):\n")
