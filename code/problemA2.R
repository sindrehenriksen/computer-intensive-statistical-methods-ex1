#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- A2
# Params
a2_alpha = 0.5
a2_n = 100000

# Simulate and calculate empirical and true means and variances
a2_samples = tibble(x = rg(a2_n, a2_alpha))
a2_empirical_mean = mean(a2_samples$x)
a2_empirical_var = var(a2_samples$x)
a2_true_mean = c_func(a2_alpha) * (1 / (a2_alpha + 1) + 2 * exp(-1))
a2_true_var = c_func(a2_alpha) * (1 / (a2_alpha + 2) + 5 * exp(-1)) - 
  a2_true_mean^2

# Plot histogram of distribution together with PDF
ggplot(data=a2_samples) +
  geom_histogram(aes(x=x, y=..density..), bins=100, colour="white", 
                 fill="cornflowerblue") +
  stat_function(fun=function(x)(g(x, a2_alpha)), color="darkred",
                xlim=c(0.05, max(a2_samples$x)))

# Compare empirical means and variances with true values for different alphas
a2_empirical_means = a2_empirical_vars = numeric(9)
a2_true_means = a2_true_vars = numeric(9)
a2_alphas = seq(0.1, 0.9, 0.1)
for(i in 1:9) {
  a2_samples_i = tibble(x = rg(a2_n, a2_alphas[i]))
  a2_empirical_means[i] = mean(a2_samples_i$x)
  a2_empirical_vars[i] = var(a2_samples_i$x)
  a2_true_means[i] = c_func(a2_alphas[i]) * (1 / (a2_alphas[i] + 1) + 
                                               2 * exp(-1))
  a2_true_vars[i] = c_func(a2_alphas[i]) * (1 / (a2_alphas[i] + 2) +
                                              5 * exp(-1)) - a2_true_means[i]^2
}
max((a2_empirical_means - a2_true_means) / a2_true_means)
max((a2_empirical_vars - a2_true_vars) / a2_true_vars)
