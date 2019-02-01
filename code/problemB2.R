#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- B2
set.seed(123)

# Params
alpha = 5
n = 10000

# Simulate and calculate empirical and true means and variances
sim = r_gamma2(n, alpha)
samples = tibble(x = sim$x)
empirical_mean = mean(samples$x)
empirical_var = var(samples$x)
true_mean = true_var = alpha

# Plot histogram of distribution together with PDF
ggplot(data = samples) +
  geom_histogram(
    aes(x = x, y = ..density..),
    bins = 50,
    colour = "white",
    fill = "cornflowerblue"
  ) +
  stat_function(
    fun = function(x)
      dgamma(x, shape = alpha),
    colour = "darkred"
  )

# Compare empirical means and variances with true values for different alphas
n = 1000
alphas = c(1.1,
           2,
           5,
           10,
           20,
           35,
           50,
           75,
           100,
           200,
           350,
           500,
           750,
           1000,
           1250,
           1500,
           1750,
           2000)
empirical_means = empirical_vars = n_tries = numeric(length(alphas))
for (i in 1:length(alphas)) {
  sim_i = r_gamma2(n, alphas[i])
  n_tries[i] = sim_i$n_tries
  x = sim_i$x
  empirical_means[i] = mean(x)
  empirical_vars[i] = var(x)
}
true_means = true_vars = alphas
max((empirical_means - true_means) / true_means)
max((empirical_vars - true_vars) / true_vars)

# Plot number of tries needed to generate 1000 realizations versus alpha
ggplot(data = tibble(x = alphas, y = n_tries)) +
  geom_point(aes(x = x, y = y)) + xlab("alpha") + ylab("tries")

# Log-log plot of number of tries versus alpha
ggplot(data = tibble(x = alphas, y = n_tries), aes(x = x, y = y)) +
  geom_point() +
  stat_function(
    fun = function(x)
      (log10(500 * sqrt(x))),
    colour = "darkred",
    geom = "line"
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x)
      (10 ^ x)),
    labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x)
      (10 ^ x)),
    labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
  ) +
  annotation_logticks() +
  xlab("alpha") + ylab("tries")
