# Marketing Regression Analysis

# 1. Load Packages

library(datarium)
library(ggplot2)
library(car)


# 2. Load Data

data("marketing")
head(marketing)

# 3. Exploratory Data Analysis
summary(marketing)
cor(marketing)

# 4. Fit Multiple Regression Model

model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model)


# 5. Model Diagnostics

# Residual Plot
plot(model$fitted.values, model$residuals,
     pch = 19, xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Multicollinearity Check
vif(model)


# 6. Predictions

new_data <- data.frame(youtube = 200, facebook = 40, newspaper = 20)
predict(model, newdata = new_data, interval = "confidence")


# 7. Save Visualizations

# Sales vs YouTube
p1 <- ggplot(marketing, aes(youtube, sales)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", color = "darkred") +
  theme_minimal() +
  labs(title = "Sales vs YouTube Advertising", x = "YouTube Budget", y = "Sales")
ggsave("images/sales_vs_youtube.png", plot = p1)

# Predicted vs Actual
marketing$predicted <- predict(model)
p2 <- ggplot(marketing, aes(predicted, sales)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "Predicted vs Actual Sales", x = "Predicted", y = "Actual")
ggsave("images/predicted_vs_actual.png", plot = p2)
