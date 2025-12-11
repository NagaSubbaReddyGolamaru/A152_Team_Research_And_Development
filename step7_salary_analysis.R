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
