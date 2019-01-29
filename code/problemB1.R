#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- B1
set.seed(123)

# Params
alpha = 0.5
n = 10000

# Simulate and calculate empirical and true means and variances
samples = tibble(x = r_gamma1(n, alpha))
empirical_mean = mean(samples$x)
empirical_var = var(samples$x)
true_mean = true_var = alpha

# Plot histogram of distribution together with PDF
ggplot(data=samples) +
  geom_histogram(aes(x=x, y=..density..), bins=100, colour="white",
                 fill="cornflowerblue", show.legend=TRUE) +
  stat_function(fun=function(x)f_gamma1(x, alpha), colour="darkred",
                xlim=c(0.05, max(samples$x)))
  #stat_function(fun=function(x)dgamma(x, shape=alpha),
  #              xlim=c(0.05, max(samples$x)))

# Compare empirical means and variances with true values for different alphas
empirical_means = empirical_vars = numeric(9)
alphas = seq(0.1, 0.9, 0.1)
for(i in 1:9) {
  x = r_gamma1(n, alphas[i])
  empirical_means[i] = mean(x)
  empirical_vars[i] = var(x)
}
true_means = true_vars = alphas
max((empirical_means - true_means) / true_means)
max((empirical_vars - true_vars) / true_vars)
