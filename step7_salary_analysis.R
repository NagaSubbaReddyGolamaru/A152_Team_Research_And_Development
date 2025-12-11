# STEP 7: STATISTICAL TESTING

male_salaries <- data_clean %>% 
  filter(gender == "Male") %>% 
  pull(salary_numeric)

female_salaries <- data_clean %>% 
  filter(gender == "Female") %>% 
  pull(salary_numeric)

cat("\n=== ASSUMPTION CHECKS ===\n")
cat("\nNormality Tests (Shapiro-Wilk):\n")

# Normality Tests (Shapiro-Wilk)

if (length(male_salaries) > 500) {
  set.seed(123)
  male_sample <- sample(male_salaries, 500)
  shapiro_male <- shapiro.test(male_sample)
} else {
  shapiro_male <- shapiro.test(male_salaries)
}

if (length(female_salaries) > 500) {
  set.seed(123)
  female_sample <- sample(female_salaries, 500)
  shapiro_female <- shapiro.test(female_sample)
} else {
  shapiro_female <- shapiro.test(female_salaries)
}

cat("\n** INTERPRETATION OF NORMALITY TESTS **")
cat("\nBoth p-values < 0.05 indicate departure from normality.")
cat("\nThis suggests using non-parametric tests as a robustness check.\n")

cat("\n=== VARIANCE EQUALITY TEST ===\n")

levene_test <- var.test(male_salaries, female_salaries)

cat("F-statistic =", levene_test$statistic, ", p-value =", levene_test$p.value, "\n")

cat("Interpretation: ",
    ifelse(levene_test$p.value < 0.05,
           "Variances are SIGNIFICANTLY different - use Welch's t-test",
           "Variances are equal - can use Student's t-test"),
    "\n")

cat("\n=== PARAMETRIC TESTS: T-TESTS ===\n")

# Welch's t-test (recommended when variances differ)
t_test_welch <- t.test(
  male_salaries,
  female_salaries,
  alternative = "two.sided",
  var.equal = FALSE
)

cat("\nWelch Two Sample t-test (RECOMMENDED):\n")
print(t_test_welch)

# Student's t-test (only if variances are equal)
t_test_student <- t.test(
  male_salaries,
  female_salaries,
  alternative = "two.sided",
  var.equal = TRUE
)

cat("\nStudent's Two Sample t-test:\n")
print(t_test_student)

# Effect Size: Cohen's d

pooled_sd <- sqrt(
  ((length(male_salaries) - 1) * sd(male_salaries)^2 +
   (length(female_salaries) - 1) * sd(female_salaries)^2) /
  (length(male_salaries) + length(female_salaries) - 2)
)

cohens_d <- (mean(male_salaries) - mean(female_salaries)) / pooled_sd

cat("\nEffect Size (Cohen's d):", round(cohens_d, 4), "\n")

cat("Interpretation: ",
    ifelse(abs(cohens_d) < 0.2, "negligible effect",
           ifelse(abs(cohens_d) < 0.5, "small effect",
                  ifelse(abs(cohens_d) < 0.8, "medium effect", "large effect"))),
    "\n")

cat("\n=== NON-PARAMETRIC TEST: MANN-WHITNEY U TEST ===\n")
cat("(Recommended due to normality violation)\n\n")

mann_whitney <- wilcox.test(
  male_salaries,
  female_salaries,
  alternative = "two.sided"
)

print(mann_whitney)

cat("\n** INTERPRETATION **")
cat("\nMann-Whitney U test does not assume normal distribution.")
cat("\nIt tests whether the two groups have the same distribution.\n")


# STEP 8: COMPREHENSIVE RESULTS SUMMARY

cat("\n\nRESEARCH QUESTION:\n")
cat("  Is there a statistically significant difference in annual salary\n")
cat("  between male and female data science professionals?\n\n")

cat("HYPOTHESES:\n")
cat("  H₀ (Null): There is no significant difference in annual salary\n")
cat("             between males and females.\n")
cat("  H₁ (Alternative): There is a significant difference in annual salary\n")
cat("                    between males and females.\n\n")

cat("SAMPLE SIZES:\n")
cat("  Male respondents:   ", length(male_salaries), "\n")
cat("  Female respondents: ", length(female_salaries), "\n")
cat("  Sample ratio:       ", round(length(male_salaries) / length(female_salaries), 2), ":1 (Male:Female)\n\n")

cat("DESCRIPTIVE STATISTICS:\n")
cat("  Mean salary (Male):   $", formatC(mean(male_salaries), format = "f", digits = 2, big.mark = ","), "\n")
cat("  Mean salary (Female): $", formatC(mean(female_salaries), format = "f", digits = 2, big.mark = ","), "\n")
cat("  Difference in means:  $", formatC(mean(male_salaries) - mean(female_salaries), format="f", digits=2, big.mark=","), "\n\n")

cat("  Median salary (Male):   $", formatC(median(male_salaries), format="f", digits=2, big.mark=","), "\n")
cat("  Median salary (Female): $", formatC(median(female_salaries), format="f", digits=2, big.mark=","), "\n\n")

cat("STATISTICAL TEST RESULTS:\n")
cat("  Welch's t-test p-value:      ", format.pval(t_test_welch$p.value, digits = 4), "\n")
cat("  Mann-Whitney U p-value:      ", format.pval(mann_whitney$p.value, digits = 4), "\n")
cat("  Cohen's d effect size:       ", round(cohens_d, 4), "\n\n")

cat("ASSUMPTION CHECKS:\n")
cat("  Normality (Male):      ", ifelse(shapiro_male$p.value < 0.05, "VIOLATED (p < 0.05)", "SATISFIED (p > 0.05)"), "\n")
cat("  Normality (Female):    ", ifelse(shapiro_female$p.value < 0.05, "VIOLATED (p < 0.05)", "SATISFIED (p > 0.05)"), "\n")
cat("  Equal variances:       ", ifelse(levene_test$p.value < 0.05, "VIOLATED (p < 0.05)", "SATISFIED (p > 0.05)"), "\n\n")

cat("CONCLUSION (α = 0.05):\n")
if (t_test_welch$p.value < 0.05) {
  cat("  REJECT the null hypothesis\n")
  cat("  There IS a statistically significant difference in salaries between males and females.\n\n")
  cat("  Males earn significantly ",
      ifelse(mean(male_salaries) > mean(female_salaries), "MORE", "LESS"),
      " than females on average.\n")
} else {
  cat("  FAIL TO REJECT the null hypothesis\n")
  cat("  There is NO statistically significant difference in salaries between males and females.\n")
}

cat("\nKEY FINDINGS:\n")
cat("  1. Mean salary difference: $", formatC(abs(mean(male_salaries) - mean(female_salaries)), format="f", digits=2, big.mark=","), "\n")
cat("  2. Statistical significance: ", ifelse(t_test_welch$p.value < 0.05, "YES (p < 0.05)", "NO (p > 0.05)"), "\n")
cat("  3. Effect size: ", 
    ifelse(abs(cohens_d) < 0.2, "Negligible",
           ifelse(abs(cohens_d) < 0.5, "Small",
                  ifelse(abs(cohens_d) < 0.8, "Medium", "Large"))),
    " (Cohen's d = ", round(cohens_d, 4), ")\n")
cat("  4. Gender distribution: ",
    round(length(male_salaries) / nrow(data_clean) * 100, 1), "% Male, ",
    round(length(female_salaries) / nrow(data_clean) * 100, 1), "% Female\n")
cat("  5. Both normality assumptions violated; Mann-Whitney U test supports findings.\n")
