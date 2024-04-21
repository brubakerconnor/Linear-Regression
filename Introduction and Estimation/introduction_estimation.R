x <- seq(0, 10, by = 1)
y1 <- -0.3 * x + 1
y2 <- 1.4 * x - 3

pdf("Lectures/Linear Regression/Introduction and Estimation/figures/perfect_linear.pdf",
    width = 8, height = 4)
old.par <- par(mfrow = c(1, 2))
plot(x, y1, type = 'o', pch = 16, ylim = c(-8, 1)); grid()
plot(x, y2, type = 'o', pch = 16, ylim = c(-3, 11)); grid()
par(old.par)
graphics.off()

library(modeldata)
data("Sacramento")
pdf("Lectures/Linear Regression/Introduction and Estimation/figures/sacramento.pdf",
    width = 8, height = 6)
plot(Sacramento$sqft, Sacramento$price, xlab = "Square Footage", ylab = "Price",
     main = "Housing Prices in Sacramento, CA"); grid()
graphics.off()


lm1 <- lm(price ~ sqft, data = Sacramento)
pdf("Lectures/Linear Regression/Introduction and Estimation/figures/sacramento_lines.pdf",
    width = 8, height = 6)
plot(Sacramento$sqft, Sacramento$price, xlab = "Square Footage", ylab = "Price",
     main = "Housing Prices in Sacramento, CA"); grid()
abline(lm1, col = 2, lwd = 2)
abline(a = 13900, b = 125, col = 3, lwd = 2)
abline(a = 13900, b = 155, col = 4, lwd = 2)
graphics.off()

pdf("Lectures/Linear Regression/Introduction and Estimation/figures/sacramento_ls.pdf",
    width = 6, height = 6)
plot(Sacramento$sqft, Sacramento$price, xlab = "Square Footage", ylab = "Price",
     main = "Housing Prices in Sacramento, CA"); grid()
abline(lm1, col = 2, lwd = 2)
graphics.off()

sxy <- function(x, y) {
  sum((x - mean(x)) * (y - mean(y)))
}
pdf("Lectures/Linear Regression/Introduction and Estimation/figures/covariance.pdf",
    width = 12, height = 8)
set.seed(8)
x <- sort(5 * runif(50))
y1 <- x + rnorm(length(x), sd = 0.2)
y2 <- x + rnorm(length(x), sd = 1.5)
y3 <- rnorm(length(x))

y4 <- -x + rnorm(length(x), sd = 0.2)
y5 <- -x + rnorm(length(x), sd = 1.5)
y6 <- sin(x) + rnorm(length(x), sd = 0.1)
old.par <- par(mfrow = c(2, 3))
plot(x, y1, xlab = "X", ylab = "Y", main = "Strong Positive Covariance")
plot(x, y2, xlab = "X", ylab = "Y", main = "Weak Positive Covariance")
plot(x, y3, xlab = "X", ylab = "Y", main = "No Covariance")
plot(x, y4, xlab = "X", ylab = "Y", main = "Strong Negative Covariance")
plot(x, y5, xlab = "X", ylab = "Y", main = "Weak Negative Covariance")
plot(x, y6, xlab = "X", ylab = "Y", main = "No Covariance (Non-Linear)")
graphics.off()

library(ggplot2)
model <- lm(price ~ sqft, data = Sacramento)
Sacramento$fitted_price <- model$fitted.values
p1 <- ggplot(Sacramento, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.7) +  # Plot data points
  geom_point(aes(x = sqft, y = price), color = 'grey') +
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "red") +  # Plot regression line
  # geom_hline(yintercept = mean_y, color = "green", linetype = "dashed") +  # Plot mean of y line
  geom_segment(aes(xend = sqft, yend = fitted_price), color = "purple", linetype = "dotted") +  # Lines for regression sum of squares (ESS)
  labs(title = "Residuals",
       x = "Square Footage (x)",
       y = "Price (y)") +
  theme_minimal()
ggsave("Lectures/Linear Regression/Introduction and Estimation/figures/housing_resid.png", width = 8, height = 6)

