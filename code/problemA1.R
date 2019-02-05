#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- A1
set.seed(123)

# Params
lambda = 0.5
n = 10000

# Simulate from r_exp and create a dataframe
samples = enframe(r_exp(n, lambda))

# Plot histogram of distribution together with PDF
ggplot(data = samples) +
  geom_histogram(
    aes(x = value, y = ..density..),
    bins = 50,
    colour = "white",
    fill = "cornflowerblue"
  ) +
  stat_function(
    fun = function(x)
      dexp(x, lambda),
    color = "darkred"
  )

# Compare empirical means and variances with true values for different alphas
n_lambdas = 9
lambdas_start = 1
lambdas_stop = 50
empirical_means = empirical_vars = numeric(n_lambdas)
true_means = true_vars = numeric(n_lambdas)
lambdas = seq(lambdas_start,
              lambdas_stop,
              (lambdas_stop - lambdas_start) / (n_lambdas - 1))
for (i in 1:n_lambdas) {
  samples_i = enframe(x = r_exp(n, lambdas[i]))
  empirical_means[i] = mean(samples_i$value)
  empirical_vars[i] = var(samples_i$value)
  true_means[i] = 1 / lambdas[i]
  true_vars[i] = 1 / lambdas[i] ^ 2
}
max((empirical_means - true_means) / true_means)
max((empirical_vars - true_vars) / true_vars)
