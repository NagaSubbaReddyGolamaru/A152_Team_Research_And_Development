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

# STEP 5: VISUALIZATIONS – HISTOGRAMS

# Histogram for Male Salaries
hist_male <- ggplot(data_clean %>% filter(gender == "Male"), 
                    aes(x = salary_numeric)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "steelblue",
                 color = "black",
                 alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(data_clean$salary_numeric[data_clean$gender == "Male"], na.rm = TRUE),
                  sd = sd(data_clean$salary_numeric[data_clean$gender == "Male"], na.rm = TRUE)
                ),
                color = "red",
                linewidth = 1.2) +
  labs(
    title = "Distribution of Annual Salary - Male",
    subtitle = "Normal Distribution Curve (Red)",
    x = "Annual Salary (USD)",
    y = "Density"
  ) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  theme_minimal()

print(hist_male)


# Histogram for Female Salaries
hist_female <- ggplot(data_clean %>% filter(gender == "Female"), 
                      aes(x = salary_numeric)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "coral",
                 color = "black",
                 alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(data_clean$salary_numeric[data_clean$gender == "Female"], na.rm = TRUE),
                  sd = sd(data_clean$salary_numeric[data_clean$gender == "Female"], na.rm = TRUE)
                ),
                color = "red",
                linewidth = 1.2) +
  labs(
    title = "Distribution of Annual Salary - Female",
    subtitle = "Normal Distribution Curve (Red)",
    x = "Annual Salary (USD)",
    y = "Density"
  ) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  theme_minimal()

print(hist_female)