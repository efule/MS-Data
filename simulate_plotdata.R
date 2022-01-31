# Simulate and plot data
# Emily Fule
# efuel@email.arizona.edu
# 2022-01-31

# Simulate predictor variable
x <- rnorm(n = 100)
# Simulate response variable with some noise
y <- 2 * x + rnorm(n = 100, sd = 0.2)

# Plot the data
plot(x = x, y = y)