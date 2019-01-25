#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- B1
# Params
b1_alpha = 0.5
b1_n = 10000

# Simulate and calculate empirical and true means and variances
b1_samples = tibble(x = rf1(b1_n, b1_alpha))
b1_empirical_mean = mean(b1_samples$x)
b1_empirical_var = var(b1_samples$x)
b1_true_mean = b1_true_var = b1_alpha

# Plot histogram of distribution together with PDF
ggplot(data=b1_samples) + 
  geom_histogram(aes(x=x, y=..density..), bins=100, colour="white", 
                 fill="cornflowerblue", show.legend=TRUE) +
  stat_function(fun=function(x)f(x, b1_alpha), colour="darkred", 
                xlim=c(0.05, max(b1_samples$x)))
  #stat_function(fun=function(x)dgamma(x, shape=b1_alpha),
  #              xlim=c(0.05, max(b1_samples$x)))

# Compare empirical means and variances with true values for different alphas
b1_empirical_means = b1_empirical_vars = numeric(9)
b1_alphas = seq(0.1, 0.9, 0.1)
for(i in 1:9) {
  b1_samples_i = tibble(x = rf1(b1_n, b1_alphas[i]))
  b1_empirical_means[i] = mean(b1_samples_i$x)
  b1_empirical_vars[i] = var(b1_samples_i$x)
}
b1_true_means = b1_true_vars = b1_alphas
max((b1_empirical_means - b1_true_means) / b1_true_means)
max((b1_empirical_vars - b1_true_vars) / b1_true_vars)
