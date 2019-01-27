#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- B3
set.seed(123)

# Params
alpha = beta = 5
n = 10000

# Simulate and calculate empirical and true means and variances
samples = tibble(x = rf(n, alpha, beta))
empirical_mean = mean(samples$x)
empirical_var = var(samples$x)
true_mean = alpha / beta
true_var = alpha / beta^2

# Plot histogram of distribution together with PDF
ggplot(data=samples) +
  geom_histogram(aes(x=x, y=..density..), bins=100, colour="white",
                 fill="cornflowerblue") +
  stat_function(fun=function(x)dgamma(x, shape=alpha, rate=beta),
                colour="darkred")

# Compare empirical means and variances with true values for different alphas
empirical_means = empirical_vars = numeric(25)
true_means = true_vars = numeric(25)
alphas = c(0.5, 2, 5, 20, 50)
betas = c(0.5, 2, 5, 20, 50)
for(i in 1:25) {
  a = ((i - 1) %% 5) + 1
  b = ceiling(i / 5)
  samples_i = tibble(x = rf(n, alphas[a], betas[b]))
  empirical_means[i] = mean(samples_i$x)
  empirical_vars[i] = var(samples_i$x)
  true_means[i] = alphas[a] / betas[b]
  true_vars[i] = alphas[a] / betas[b]^2
}
max((empirical_means - true_means) / true_means)
max((empirical_vars - true_vars) / true_vars)
