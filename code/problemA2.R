#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- A2
set.seed(123)

# Params
alpha = 0.5
n = 100000

# Simulate from rg() and create a dataframe
samples = tibble(x = rg(n, alpha))

# Plot histogram of distribution together with PDF
ggplot(data = samples) +
  geom_histogram(
    aes(x = x, y = ..density..),
    bins = 100,
    colour = "white",
    fill = "cornflowerblue"
  ) +
  stat_function(
    fun = function(x)
      (g(x, alpha)),
    color="darkred",
    xlim=c(0.05, max(samples$x)))

# Compare empirical means and variances with theoretical for different alphas
empirical_means = empirical_vars = numeric(9)
true_means = true_vars = numeric(9)
alphas = seq(0.1, 0.9, 0.1)
for (i in 1:9) {
  samples_i = tibble(x = rg(n, alphas[i]))
  empirical_means[i] = mean(samples_i$x)
  empirical_vars[i] = var(samples_i$x)
  true_means[i] = c_func(alphas[i]) * (1 / (alphas[i] + 1) +
                                         2 * exp(-1))
  true_vars[i] = c_func(alphas[i]) * (1 / (alphas[i] + 2) +
                                        5 * exp(-1)) - true_means[i] ^2
}
max((empirical_means - true_means) / true_means)
max((empirical_vars - true_vars) / true_vars)
