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

# Boxplot comparison between Male and Female Salaries
boxplot_comparison <- ggplot(data_clean, aes(x = gender_group, y = salary_numeric, 
                                             fill = gender_group)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Male" = "steelblue", 
                               "Female" = "coral")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(title = "Annual Salary Comparison: Male vs Female",
       x = "Gender",
       y = "Annual Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

print(boxplot_comparison)

# Violin plot for Salary Distribution (Male vs Female)
violin_comparison <- ggplot(data_clean, aes(x = gender_group, y = salary_numeric, 
                                            fill = gender_group)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_fill_manual(values = c("Male" = "steelblue", 
                               "Female" = "coral")) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(title = "Salary Distribution Comparison with Violin Plot",
       subtitle = "Male vs Female Data Scientists",
       x = "Gender",
       y = "Annual Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

print(violin_comparison)