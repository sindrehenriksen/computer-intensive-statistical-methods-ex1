#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- A4
set.seed(123)

# Params
d = 5
n = 10000

# Simulate and calculate empirical and true means and variances
samples = r_multinorm(n, d)
empirical_mean = rowMeans(samples$y)
empirical_cov = cov(t(samples$y))
max((empirical_mean - samples$true_mean) / samples$true_mean)
max((empirical_cov - samples$true_cov) / samples$true_cov)
