# Load necessary library
library(ggplot2)

# Create a sample dataset
set.seed(42)
n <- 100
x <- rnorm(n)
y <- 3 * x + rnorm(n)

# Fit a simple linear regression model
model <- lm(y ~ x)

# Calculate the mean of the dependent variable
mean_y <- mean(y)

# Create a data frame with observed values, fitted values, and mean of y
data <- data.frame(x = x, y = y, fitted = model$fitted.values, mean_y = mean_y)

# Create the plot
p1 <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.7) +  # Plot data points
  # geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "red") +  # Plot regression line
  geom_hline(yintercept = mean_y, color = "green", linetype = "dashed") +  # Plot mean of y line
  geom_segment(aes(xend = x, yend = mean_y), color = "orange", linetype = "dotted") +  # Lines for TSS
  labs(title = "Total Sum of Squares (SST)",
       x = "Predictor (x)",
       y = "Response (y)") +
  theme_minimal()
ggsave("Lectures/Linear Regression/Inference/figures/sst.png", width = 6, height = 6)

# Load necessary library
library(ggplot2)

# Create a sample dataset
set.seed(42)
n <- 100
x <- rnorm(n)
y <- 3 * x + rnorm(n)

# Fit a simple linear regression model
model <- lm(y ~ x)

# Calculate the mean of the dependent variable
mean_y <- mean(y)

# Create a data frame with observed values, fitted values, and mean of y
data <- data.frame(x = x, y = y, fitted = model$fitted.values, mean_y = mean_y)

# Create the plot
p2 <- ggplot(data, aes(x = x, y = fitted)) +
  geom_point(color = "blue", alpha = 0.7) +  # Plot data points
  geom_point(aes(x = x, y = y), color = 'grey') +
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "red") +  # Plot regression line
  geom_hline(yintercept = mean_y, color = "green", linetype = "dashed") +  # Plot mean of y line
  geom_segment(aes(xend = x, yend = mean_y), color = "purple", linetype = "dotted") +  # Lines for regression sum of squares (ESS)
  labs(title = "Regression Sum of Squares (SSR) - Larger",
       x = "Predictor (x)",
       y = "Response (y)") +
  theme_minimal()
ggsave("Lectures/Linear Regression/Inference/figures/ssr.png", width = 6, height = 6)

# Load necessary library
library(ggplot2)

# Create a sample dataset
set.seed(42)
n <- 100
x <- rnorm(n)
y <- 0.1 * x + rnorm(n)

# Fit a simple linear regression model
model <- lm(y ~ x)

# Calculate the mean of the dependent variable
mean_y <- mean(y)

# Create a data frame with observed values, fitted values, and mean of y
data <- data.frame(x = x, y = y, fitted = model$fitted.values, mean_y = mean_y)

# Create the plot
p2b <- ggplot(data, aes(x = x, y = fitted)) +
  geom_point(color = "blue", alpha = 0.7) +  # Plot data points
  geom_point(aes(x = x, y = y), color = 'grey') +
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "red") +  # Plot regression line
  geom_hline(yintercept = mean_y, color = "green", linetype = "dashed") +  # Plot mean of y line
  geom_segment(aes(xend = x, yend = mean_y), color = "purple", linetype = "dotted") +  # Lines for regression sum of squares (ESS)
  labs(title = "Regression Sum of Squares (RSS) - Smaller",
       x = "Predictor (x)",
       y = "Response (y)") +
  ylim(-5, 5) +
  theme_minimal()
ggsave("Lectures/Linear Regression/Inference/figures/ssr_small.png", width = 6, height = 6)

# Load necessary library
library(ggplot2)

# Create a sample dataset
set.seed(42)
n <- 100
x <- rnorm(n)
y <- 3 * x + rnorm(n)

# Fit a simple linear regression model
model <- lm(y ~ x)

# Calculate the mean of the dependent variable
mean_y <- mean(y)

# Create a data frame with observed values, fitted values, and mean of y
data <- data.frame(x = x, y = y, fitted = model$fitted.values, mean_y = mean_y)

# Create the plot
p3 <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.7) +  # Plot data points
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "red") +  # Plot regression line
  geom_hline(yintercept = mean_y, color = "green", linetype = "dashed") +  # Plot mean of y line
  geom_segment(aes(xend = x, yend = fitted), color = "purple", linetype = "dotted") +  # Lines for regression sum of squares (ESS)
  labs(title = "Residual Sum of Squares (SSE) - Smaller",
       x = "Predictor (x)",
       y = "Response (y)") +
  theme_minimal()
ggsave("Lectures/Linear Regression/Inference/figures/sse.png", width = 6, height = 6)

# Load necessary library
library(ggplot2)

# Create a sample dataset
set.seed(42)
n <- 100
x <- rnorm(n)
y <- 3 * x + rnorm(n, sd = 4)

# Fit a simple linear regression model
model <- lm(y ~ x)

# Calculate the mean of the dependent variable
mean_y <- mean(y)

# Create a data frame with observed values, fitted values, and mean of y
data <- data.frame(x = x, y = y, fitted = model$fitted.values, mean_y = mean_y)

# Create the plot
p3a <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.7) +  # Plot data points
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "red") +  # Plot regression line
  geom_hline(yintercept = mean_y, color = "green", linetype = "dashed") +  # Plot mean of y line
  geom_segment(aes(xend = x, yend = fitted), color = "purple", linetype = "dotted") +  # Lines for regression sum of squares (ESS)
  labs(title = "Residual Sum of Squares (SSE) - Larger",
       x = "Predictor (x)",
       y = "Response (y)") +
  theme_minimal()
ggsave("Lectures/Linear Regression/Inference/figures/sse_larger.png", width = 6, height = 6)
