#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")
library(ggplot2)
library(cowplot)

## ---- A3
set.seed(123)

# Params
n = 50000

# Simulate and calculate empirical and true means and variances
samples = r_boxmuller(n)
empirical_mean = mean(samples$y1)
empirical_var = var(samples$y1)

# Plot histogram of distribution together with the normal distribution N(0,1)
norm1 = ggplot(data = samples) +
  geom_histogram(
    aes(x = y1, y = ..density..),
    binwidth = 0.2,
    colour = "white",
    fill = "cornflowerblue",
    size = 0.1
  ) +
  stat_function(
    fun = function(x)
      dnorm(x, 0, 1),
    color = "darkred",
    size = 1
  )

norm2 = ggplot(data = samples) +
  geom_histogram(
    aes(x = y2, y = ..density..),
    binwidth = 0.2,
    colour = "white",
    fill = "cornflowerblue",
    size = 0.1
  ) +
  stat_function(
    fun = function(x)
      dnorm(x, 0, 1),
    color = "darkred",
    size = 1
  )

plot_grid(norm1, norm2, labels = "AUTO")
