#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- D1
set.seed(123)

# Params
n = 10000

# Simulate and calculate empirical and true means and variances
#sim = r_posterior(n)
y = c(125, 18, 20, 34)
sim = r_posterior(n, y)
sim_approx = r_posterior_approx(n, y)
n_random_numbers = sim$n_random_numbers
n_random_numbers_approx = sim_approx$n_random_numbers
samples = tibble(x = sim$x, x_approx = sim_approx$x)
empirical_mean = mean(samples$x)
empirical_mean_approx = mean(samples$x_approx)
c = integrate(function(x)
  (f_posterior_star(x, y)),
  lower = 0,
  upper = 1)$value
f_posterior = function(x)
  (f_posterior_star(x, y) / c)
true_mean = integrate(function(x)
  (x * f_posterior(x)), lower = 0,
  upper = 1)$value

# Plot histogram of distribution together with PDF
ggplot(data = samples) +
  geom_histogram(
    aes(x = x, y = ..density.., col = 0),
    bins = 50,
    colour = "white",
    fill = "cornflowerblue",
    alpha = 0.5
  ) +
  geom_histogram(
    aes(x = x_approx, y = ..density.., col = 0),
    bins = 50,
    colour = "white",
    fill = "grey",
    alpha = 0.5
  ) +
  stat_function(
    fun = function(x)
      f_posterior(x),
    colour = "darkred",
    xlim = c(0.05, max(samples$x))
  ) +
  geom_vline(xintercept = empirical_mean,
             col = "red",
             size = 1) +
  geom_vline(xintercept = true_mean,
             col = "darkgreen",
             size = 0.5)

# Use importance sampling to estimate the posterior mean with prior Beta(1,5)
mean_is = posterior_mean_is(n, y)
