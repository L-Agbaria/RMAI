if (!requireNamespace("truncnorm", quietly = TRUE)) {
  install.packages("truncnorm")
}

library(truncnorm)


modified_round <- function(value) {
  # Round values close to 0.05 (from above) down.
  if (0.05 < value && value < 0.06) {
    return (0.05)
  } else {
    return (value)
  }
}

# PART B
sample_size = 237
# Number of times to repeat the experiment
tests_num = 50000
# P values extracted from each repetition of the experiment 
p_values = numeric(tests_num) 
# Creates a reproducible environment
set.seed(42) 


# Take a sample from a truncated normal distribution for explanatory variable
x_grade_stats = rtruncnorm(sample_size, a=5.5, b=9.5, mean=6.5, sd=2)
# Generate the random noise from a truncated normal distribution
random_noise = rtruncnorm(sample_size, a=-7, b=7, mean=0, sd=2)
# Generate discrete data 
y_performance = round(7 + 0 * x_grade_stats + random_noise, digits=0)

# Fit the linear regression model
lm_fit = lm(y_performance ~ x_grade_stats)


# Plot the data with the linear function
plot(x_grade_stats, 
     y_performance,
     main = "The linear regression model fitting the data",
     xlab = "Average grade on the statistic courses",
     ylab = "Number of correct decisions",
     xaxt = "n",
     yaxt = "n",
     ylim = c(0, 14)
)

# Define custom ticks for x and y axes
x_ticks = seq(from=5.5, to=9.5, by=0.5)
y_ticks = seq(from=0, to=14, by=1)

# Add custom tick marks to the x-axis and y-axis
axis(1, at=x_ticks)
axis(2, at=y_ticks)

# Add fitted regression line to scatterplot
abline(lm_fit)

lm_fit
summary(lm_fit)


# Run tests_num amount of repetitions and push the observed
# p-values to the p_values table
for(i in 1:tests_num) {
  x_grade_stats = rtruncnorm(sample_size, a=5.5, b=9.5, mean=6.5, sd=2)
  random_noise = rtruncnorm(sample_size, a=-7, b=7, mean=0, sd=2)
  y_performance = round(7 + 0 * x_grade_stats + random_noise, digits=0)
  
  # Linear regression model 
  lm_fit = lm(y_performance ~ x_grade_stats)
  
  # Summary of the linear regression model
  lm_summary = summary(lm_fit)
  
  # Contains the p-value
  p_values[i] = lm_summary$coefficients["x_grade_stats", "Pr(>|t|)"]
}

# Plot the histogram
hist(p_values,
     main = "Histogram of P values",
     xlab="P values")


# PART C (Rounding p-values down)
# Do the same experiments but round down the observed P-value 
for(i in 1:tests_num) {
  x_grade_stats = rtruncnorm(sample_size, a=5.5, b=9.5, mean=6.5, sd=2)
  random_noise = rtruncnorm(sample_size, a=-7, b=7, mean=0, sd=2)
  y_performance = round(7 + 0 * x_grade_stats + random_noise, digits=0)
  
  # Linear regression model 
  lm_fit = lm(y_performance ~ x_grade_stats)
  
  # Summary of the linear regression model
  lm_summary = summary(lm_fit)
  
  # Contains the p-value
  p_values[i] = modified_round(lm_summary$coefficients["x_grade_stats", "Pr(>|t|)"])
}

# Plot the histogram
hist(p_values,
     main = "Histogram of rounded down P values",
     xlab="P values")



# PART C (Sequential testing with optional stopping)
set.seed(201)
max_sample_size = 1000
p_values = numeric(max_sample_size)

# Take a sample from a truncated normal distribution for explanatory variable
x_grade_stats = rtruncnorm(max_sample_size, a=5.5, b=9.5, mean=6.5, sd=2)
# Generate the random noise from a truncated normal distribution
random_noise = rtruncnorm(max_sample_size, a=-7, b=7, mean=0, sd=2)
# Generate the data
y_performance = round(7 + 0 * x_grade_stats + random_noise, digits=0)

for(sample_size in 2:max_sample_size) {
  # Consider the results of only sample_size observations
  grade_stats = x_grade_stats[1:sample_size]
  performance = y_performance[1:sample_size]
  lm_fit = lm(performance ~ grade_stats)
  
  # Summary of the linear regression model
  lm_summary = summary(lm_fit)
  
  # Save all the P-values for a reduced samples
  p_value = lm_summary$coefficients["grade_stats", "Pr(>|t|)"]
  p_values[sample_size] = p_value
}

# Plot the trajectory of the P-values
plot(1:max_sample_size, 
     p_values,
     main = "P-value trajectory",
     xlab = "Sample size",
     ylab = "P-value",
     type="l"
)

abline(a=0.05, b=0, col="red")


# Additional plot for the first sample with significant results
n_when_significant = 700
grade_stats = x_grade_stats[1:n_when_significant]
performance = y_performance[1:n_when_significant]
lm_fit = lm(performance ~ grade_stats)

# Summary of the linear regression model
lm_summary = summary(lm_fit)
print(lm_summary$coefficients["grade_stats", "Pr(>|t|)"])

# Plot the data with the linear function
title = paste("The linear regression model for sample size", 
              n_when_significant)
plot(grade_stats, 
     performance,
     main = title,
     xlab = "Average grade on the statistic courses",
     ylab = "Number of correct decisions",
     xaxt = "n",
     yaxt = "n",
     ylim = c(0, 14),
     cex = 0.5  # Reduce point size (default is 1)
)

# Define custom ticks for x and y axes
x_ticks = seq(from=5.5, to=9.5, by=0.5)
y_ticks = seq(from=0, to=14, by=1)

# Add custom tick marks to the x-axis and y-axis
axis(1, at=x_ticks)
axis(2, at=y_ticks)

# Add fitted regression line to scatterplot
abline(lm_fit, col="red", lwd = 2)  # lwd increases line width

