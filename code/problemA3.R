#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")
library(ggplot2)

## ---- A3
set.seed(123)

# Params
n <- 50000

# Simulate and calculate empirical and true means and variances
samples = r_boxmuller(n)
empirical_mean = mean(samples$y1)
empirical_var = var(samples$y1)

# Creating ggplot variable of a histrogram of one of the samles and a normal 
# distribution N(0,1)
norm1 = ggplot(data = samples) +
  geom_histogram(
    aes(x = y1, y = ..density..),
    bins = 40,
    colour = "white",
    fill = "cornflowerblue"
  ) +
  stat_function(
    fun = function(x)
      dnorm(x, 0, 1),
    color = "darkred"
  )

# Creating ggplot variable of a histrogram of the other samle and a normal 
# distribution N(0,1)
norm2 = ggplot(data = samples) +
  geom_histogram(
    aes(x = y2, y = ..density..),
    bins = 40,
    colour = "white",
    fill = "cornflowerblue"
  ) +
  stat_function(
    fun = function(x)
      dnorm(x, 0, 1),
    color = "darkred"
  )

# Plotting the ggplot variables next to each other in columns
multiplot(norm1, norm2, cols = 2)