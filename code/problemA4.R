#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
source("functions.R")
library(ggplot2)
library(tibble)

## ---- A2
set.seed(123)

#params
d = 5
n = 10000

# Simulate and calculate empirical and true means and variances
samples <- r_multinorm(n,d)
empirical_mean = rowMeans(samples$y)
empirical_var = var(t(samples$y))
max((empirical_mean - samples$true_mean) / samples$true_mean)
max((empirical_var - samples$true_var) / samples$true_var)