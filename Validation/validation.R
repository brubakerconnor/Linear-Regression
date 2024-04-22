library(modeldata)
data("Sacramento")

lm1 <- lm(price ~ sqft, data = Sacramento)
pdf("Lectures/Linear Regression/Validation/figures/housing-resid.pdf", width = 8, height = 6)
plot(Sacramento$sqft, lm1$residuals)
graphics.off()

pdf("Lectures/Linear Regression/Validation/figures/housing-qq.pdf", width = 8, height = 6)
qqnorm(lm1$residuals); qqline(lm1$residuals)
graphics.off()

set.seed(123) # Fix R's RNG
n <- 1000 # Sample size
x <- seq(from = 1, to = n, by = 1) # x variable values
sd <- runif(n, min = 0, max = 1) # SD of error
epsilon <- rnorm(n, mean = 0, sd = sd*x) # Non-constant error
y <- x + epsilon # y values from linear predictor
lm2 <- lm(y ~ x)
pdf("Lectures/Linear Regression/Validation/figures/heteroscedasticity.pdf", width = 8, height = 6)
plot(x, lm2$residuals, xlab = "X", ylab = "Residual")
graphics.off()

# leverage examples
## good leverage point
set.seed(9)
x <- c(7.5 * runif(15), 15)
y <- 4.3 * x + 2 + rnorm(length(x), sd = 2.5)
lm1 <- lm(y ~ x)
lm2 <- lm(y[-length(y)] ~ x[-length(x)])
pdf("Lectures/Linear Regression/Validation/figures/good_leverage.pdf", width = 8, height = 6)
plot(x, y, pch = 16)
abline(lm1, col = 2)
abline(lm2, col = 4)
legend("topleft", legend = c("With Leverage Point", "Without Leverage Point"), lwd = rep(2, 2), col = c(2, 4), lty = rep(1, 2))
text(12, 64, "Good Leverage Point", col = 2)
graphics.off()

## bad leverage point
set.seed(9)
x <- c(7.5 * runif(15), 15)
y <- 4.3 * x[-length(x)] + 2 + rnorm(length(x) - 1, sd = 2.5)
y <- c(y, min(y))
lm1 <- lm(y ~ x)
lm2 <- lm(y[-length(y)] ~ x[-length(x)])
pdf("Lectures/Linear Regression/Validation/figures/bad_leverage.pdf", width = 8, height = 6)
plot(x, y, pch = 16)
abline(lm1, col = 2)
abline(lm2, col = 4)
legend("topleft", legend = c("With Leverage Point", "Without Leverage Point"), lwd = rep(2, 2), col = c(2, 4), lty = rep(1, 2))
text(13, 5, "Bad Leverage Point", col = 2)
graphics.off()
