# STEP 6: CONTINGENCY TABLE

data_clean <- data_clean %>%
  mutate(salary_category = case_when(
    salary_numeric < 30000 ~ "Low (<$30k)",
    salary_numeric >= 30000 & salary_numeric < 70000 ~ "Medium ($30k-$70k)",
    salary_numeric >= 70000 & salary_numeric < 120000 ~ "High ($70k-$120k)",
    salary_numeric >= 120000 ~ "Very High (≥$120k)"
  ))

contingency_table <- table(data_clean$gender_group, data_clean$salary_category)

cat("\n=== CONTINGENCY TABLE ===\n")
cat("Gender vs Salary Category\n\n")
print(contingency_table)

cat("\n")
print(addmargins(contingency_table))