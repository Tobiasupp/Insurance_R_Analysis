
table(insurance$smoker)

plot_smoker <- ggplot(insurance, aes(x = bmi_class, y = charges, fill = smoker)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Försäkringskostnad baserat på BMI-klass och rökning",
    x = "BMI-kategori",
    y = "Årlig kostnad (charges)",
    fill = "Rökare"
  )
  


plot_health_score <- ggplot(insurance, aes(x = health_score, y = charges)) +
  geom_jitter(alpha = 0.4, color = "steelblue") + # Jitter gör att punkterna inte hamnar exakt på varandra
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Samband mellan Health Score och försäkringskostnad",
    subtitle = "Trendlinjen visar den genomsnittliga kostnadsminskningen per poäng",
    x = "Health Score (Högre poäng = bättre livsstil)",
    y = "Årlig kostnad (charges)"
  )

