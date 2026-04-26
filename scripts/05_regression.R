# Skapa regressionsmodellen
model_1 <- lm(charges ~ smoker + age + bmi + health_score, data = insurance)

# Visa resultatet
summary(model_1)


model_summary <- summary(model_1)


