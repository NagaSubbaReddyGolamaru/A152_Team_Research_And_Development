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

# STEP 6 (Part 2): CHI-SQUARE TEST FOR INDEPENDENCE

chi_test <- chisq.test(contingency_table)

cat("\n=== CHI-SQUARE TEST FOR INDEPENDENCE ===\n")
print(chi_test)

cat("\n--- INTERPRETATION ---\n")

cat("Null Hypothesis (H0): Gender and salary category are independent.\n")
cat("Alternative Hypothesis (H1): Gender and salary category are NOT independent.\n\n")

cat("Chi-square statistic: ", round(chi_test$statistic, 4), "\n")
cat("Degrees of freedom: ", chi_test$parameter, "\n")
cat("P-value: ", chi_test$p.value, "\n\n")

if (chi_test$p.value < 0.05) {
  cat("Conclusion: Reject H0. There is a SIGNIFICANT association between gender and salary category.\n")
  cat("Interpretation: Salary levels differ between genders.\n")
} else {
  cat("Conclusion: Fail to reject H0. No statistically significant relationship between gender and salary category.\n")
  cat("Interpretation: Salary levels do NOT differ by gender in categorical distribution.\n")
}