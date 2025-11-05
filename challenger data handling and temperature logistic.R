library(alr4)
library(ggplot2)
library(sjPlot)

Challeng
write_csv(Challeng,"janitor_updatedByMC_2025.csv")

ggplot(Challeng, aes(x = temp, y = fail, color = damage)) +
  geom_point(size = 3) +
  labs(
    x = "Temperature (°F)",
    y = "Number of O-ring Failures",
    color = "Damage Level",
    title = "O-ring Failures vs Temperature (Colored by Damage)"
  ) +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")



Challeng$fail_any <- ifelse(Challeng$fail > 0, 1, 0)

model1 <- glm(fail_any ~ temp, data = Challeng, family = binomial)

summary(model1)

temp_seq <- data.frame(temp = seq(min(Challeng$temp), max(Challeng$temp), length.out = 100))
temp_seq$pred_prob <- predict(model1, newdata = temp_seq, type = "response")

# Plot sigmoid curve
ggplot(Challeng, aes(x = temp, y = fail_any)) +
  geom_point(size = 3) +
  geom_line(data = temp_seq, aes(x = temp, y = pred_prob), color = "blue", linewidth = 1) +
  labs(
    x = "Temperature (°F)",
    y = "Probability of O-ring Failure",
    title = "Logistic Regression: Probability of Failure vs Temperature"
  ) +
  theme_minimal()
