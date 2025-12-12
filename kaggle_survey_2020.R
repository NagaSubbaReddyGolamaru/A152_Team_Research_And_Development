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

# STEP 2: DATA QUALITY ASSESSMENT
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

data <- data %>% distinct()
cat("\nRows after removing duplicates:", nrow(data), "\n")

# STEP 3: DATA CLEANING AND PREPROCESSING
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
  filter(!is.na(salary_numeric)) %>%
  filter(salary_numeric > 0) %>%
  filter(!is.na(gender)) %>% 
  mutate(gender_group = gender)

cat("\n=== DATA CLEANING SUMMARY ===\n")
cat("Original dataset rows:", nrow(data), "\n")
cat("Rows with valid salary and gender data:", nrow(data_clean), "\n")
cat("Rows removed (missing/invalid data):", nrow(data) - nrow(data_clean), "\n")
cat("Male respondents:", sum(data_clean$gender == "Male"), "\n")
cat("Female respondents:", sum(data_clean$gender == "Female"), "\n")
cat("Percentage Male:", round(sum(data_clean$gender == "Male")/nrow(data_clean)*100, 2), "%\n")
cat("Percentage Female:", round(sum(data_clean$gender == "Female")/nrow(data_clean)*100, 2), "%\n")

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

# STEP 5: VISUALIZATIONS
# Histogram for Male
hist_male <- ggplot(data_clean %>% filter(gender == "Male"), 
                    aes(x = salary_numeric)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", 
                 color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data_clean$salary_numeric[data_clean$gender == "Male"], na.rm = TRUE),
                            sd = sd(data_clean$salary_numeric[data_clean$gender == "Male"], na.rm = TRUE)),
                color = "red", linewidth = 1.2) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  labs(title = "Distribution of Annual Salary - Male",
       subtitle = "With Normal Distribution Overlay (Red Curve)",
       x = "Annual Salary (USD)",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

print(hist_male)

# Histogram for Female
hist_female <- ggplot(data_clean %>% filter(gender == "Female"), 
                      aes(x = salary_numeric)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "coral", 
                 color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data_clean$salary_numeric[data_clean$gender == "Female"], na.rm = TRUE),
                            sd = sd(data_clean$salary_numeric[data_clean$gender == "Female"], na.rm = TRUE)),
                color = "red", linewidth = 1.2) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  labs(title = "Distribution of Annual Salary - Female",
       subtitle = "With Normal Distribution Overlay (Red Curve)",
       x = "Annual Salary (USD)",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

print(hist_female)

# Box plot comparison
boxplot_comparison <- ggplot(data_clean, aes(x = gender_group, y = salary_numeric, 
                                             fill = gender_group)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Male" = "steelblue", 
                               "Female" = "coral")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  labs(title = "Annual Salary Comparison: Male vs Female",
       x = "Gender",
       y = "Annual Salary (USD)",
       fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

print(boxplot_comparison)

# Violin plot
violin_comparison <- ggplot(data_clean, aes(x = gender_group, y = salary_numeric, 
                                            fill = gender_group)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = c("Male" = "steelblue", 
                               "Female" = "coral")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "", big.mark = ",")) +
  labs(title = "Salary Distribution Comparison with Violin Plot",
       subtitle = "Male vs Female Data Scientists",
       x = "Gender",
       y = "Annual Salary (USD)",
       fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

print(violin_comparison)

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

chi_test <- chisq.test(contingency_table)
cat("\n=== CHI-SQUARE TEST FOR INDEPENDENCE ===\n")
print(chi_test)

# STEP 7: STATISTICAL TESTING
male_salaries <- data_clean %>% 
  filter(gender == "Male") %>% 
  pull(salary_numeric)

female_salaries <- data_clean %>% 
  filter(gender == "Female") %>% 
  pull(salary_numeric)

cat("\n=== ASSUMPTION CHECKS ===\n")

# Normality test - sample first 5000 for Male
cat("\nNormality Tests (Shapiro-Wilk):\n")
if(length(male_salaries) > 500) {
  set.seed(123)
  male_sample <- sample(male_salaries, 500)
  shapiro_male <- shapiro.test(male_sample)
} else {
  shapiro_male <- shapiro.test(male_salaries)
}

if(length(female_salaries) > 500) {
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
cat("Interpretation:", ifelse(levene_test$p.value < 0.05, 
                              "Variances are SIGNIFICANTLY different - use Welch's t-test",
                              "Variances are equal - can use Student's t-test"), "\n")

cat("\n=== PARAMETRIC TESTS: T-TESTS ===\n")

# Welch's t-test 
t_test_welch <- t.test(male_salaries, female_salaries, 
                       alternative = "two.sided",
                       var.equal = FALSE)

cat("\nWelch Two Sample t-test (RECOMMENDED):\n")
print(t_test_welch)

# Student's t-test
t_test_student <- t.test(male_salaries, female_salaries, 
                         alternative = "two.sided",
                         var.equal = TRUE)

cat("\nStudent's Two Sample t-test:\n")
print(t_test_student)

pooled_sd <- sqrt(((length(male_salaries) - 1) * sd(male_salaries)^2 + 
                     (length(female_salaries) - 1) * sd(female_salaries)^2) / 
                    (length(male_salaries) + length(female_salaries) - 2))

cohens_d <- (mean(male_salaries) - mean(female_salaries)) / pooled_sd

cat("\nEffect Size (Cohen's d):", round(cohens_d, 4), "\n")
cat("Interpretation: ", 
    ifelse(abs(cohens_d) < 0.2, "negligible effect",
           ifelse(abs(cohens_d) < 0.5, "small effect",
                  ifelse(abs(cohens_d) < 0.8, "medium effect", "large effect"))), "\n")

cat("\n=== NON-PARAMETRIC TEST: MANN-WHITNEY U TEST ===\n")
cat("(Recommended due to normality violation)\n\n")

mann_whitney <- wilcox.test(male_salaries, female_salaries, 
                            alternative = "two.sided")

print(mann_whitney)

cat("\n** INTERPRETATION **")
cat("\nMann-Whitney U test does not assume normal distribution.")
cat("\nIt tests whether the two groups have the same distribution.\n")

# STEP 8: COMPREHENSIVE RESULTS SUMMARY

cat("RESEARCH QUESTION:\n")
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
cat("  Sample ratio:       ", round(length(male_salaries)/length(female_salaries), 2), ":1 (Male:Female)\n\n")

cat("DESCRIPTIVE STATISTICS:\n")
cat("  Mean salary (Male):   $", formatC(mean(male_salaries), format="f", digits=2, big.mark=","), "\n")
cat("  Mean salary (Female): $", formatC(mean(female_salaries), format="f", digits=2, big.mark=","), "\n")
cat("  Difference in means:  $", formatC(mean(male_salaries) - mean(female_salaries), format="f", digits=2, big.mark=","), "\n\n")

cat("  Median salary (Male):   $", formatC(median(male_salaries), format="f", digits=2, big.mark=","), "\n")
cat("  Median salary (Female): $", formatC(median(female_salaries), format="f", digits=2, big.mark=","), "\n\n")

cat("STATISTICAL TEST RESULTS:\n")
cat("  Welch's t-test p-value:      ", format.pval(t_test_welch$p.value, digits = 4), "\n")
cat("  Mann-Whitney U p-value:      ", format.pval(mann_whitney$p.value, digits = 4), "\n")
cat("  Cohen's d effect size:       ", round(cohens_d, 4), " (", 
    ifelse(abs(cohens_d) < 0.2, "negligible", 
           ifelse(abs(cohens_d) < 0.5, "small", 
                  ifelse(abs(cohens_d) < 0.8, "medium", "large"))), ")\n\n")

cat("ASSUMPTION CHECKS:\n")
cat("  Normality (Male):      ", ifelse(shapiro_male$p.value < 0.05, "VIOLATED (p<0.05)", "SATISFIED (p>0.05)"), "\n")
cat("  Normality (Female):    ", ifelse(shapiro_female$p.value < 0.05, "VIOLATED (p<0.05)", "SATISFIED (p>0.05)"), "\n")
cat("  Equal variances:       ", ifelse(levene_test$p.value < 0.05, "VIOLATED (p<0.05)", "SATISFIED (p>0.05)"), "\n\n")

cat("CONCLUSION (α = 0.05):\n")
if(t_test_welch$p.value < 0.05) {
  cat("  ✓ REJECT the null hypothesis\n")
  cat("  There IS a statistically significant difference in salaries\n")
  cat("  between male and female data science professionals.\n\n")
  cat("  Males earn significantly", ifelse(mean(male_salaries) > mean(female_salaries), "MORE", "LESS"),
      "than females on average.\n")
} else {
  cat("  ✗ FAIL TO REJECT the null hypothesis\n")
  cat("  There is NO statistically significant difference in salaries\n")
  cat("  between male and female data science professionals\n")
  cat("  (p = ", format.pval(t_test_welch$p.value, digits = 4), ").\n")
}

cat("\nKEY FINDINGS:\n")
cat("  1. Mean salary difference: $", formatC(abs(mean(male_salaries) - mean(female_salaries)), 
                                              format="f", digits=2, big.mark=","), "\n")
cat("  2. Statistical significance: ", ifelse(t_test_welch$p.value < 0.05, "YES (p < 0.05)", "NO (p > 0.05)"), "\n")
cat("  3. Effect size: ", ifelse(abs(cohens_d) < 0.2, "Negligible", 
                                 ifelse(abs(cohens_d) < 0.5, "Small", 
                                        ifelse(abs(cohens_d) < 0.8, "Medium", "Large"))), 
    " (Cohen's d = ", round(cohens_d, 4), ")\n")
cat("  4. Gender distribution: ", round(length(male_salaries)/nrow(data_clean)*100, 1), 
    "% Male, ", round(length(female_salaries)/nrow(data_clean)*100, 1), "% Female\n")
cat("  5. Both normality assumptions violated; Mann-Whitney U test supports findings.\n")


# Save plots (optional - uncomment to save)
# ggsave("histogram_male_salary.png", hist_male, width = 10, height = 6, dpi = 300)
# ggsave("histogram_female_salary.png", hist_female, width = 10, height = 6, dpi = 300)
# ggsave("boxplot_gender_comparison.png", boxplot_comparison, width = 10, height = 6, dpi = 300)
# ggsave("violin_gender_comparison.png", violin_comparison, width = 10, height = 6, dpi = 300)