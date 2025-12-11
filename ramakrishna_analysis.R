# STEP 4: DESCRIPTIVE STATISTICS
summary_stats <- data_clean %>%
  group_by(gender_group) %>%
  summarise(
    n = n(),
    mean_salary = mean(salary_numeric, na.rm = TRUE),
    median_salary = median(salary_numeric, na.rm = TRUE),
    sd_salary = sd(salary_numeric, na.rm = TRUE),
    min_salary = min(salary_numeric, na.rm = TRUE),
    max_salary = max(salary_numeric, na.rm = TRUE),
    q1_salary = quantile(salary_numeric, 0.25, na.rm = TRUE),
    q3_salary = quantile(salary_numeric, 0.75, na.rm = TRUE)
  )

cat("\n=== DESCRIPTIVE STATISTICS BY GENDER ===\n")
print(summary_stats)