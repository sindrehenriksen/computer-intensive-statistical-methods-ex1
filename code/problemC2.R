#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")

## ---- A1
set.seed(123)

# Params
K = 3
n = 10000
start_alpha = 5
stop_alpha = 10
alpha = seq(start_alpha,stop_alpha,(stop_alpha-start_alpha)/(K-1))
beta = 1

# Simulate and calculate empirical and true means and variances
samples = r_dirichlet(K,n,alpha,beta)
max((samples$empirical_mean - samples$true_mean) / samples$true_mean)
max((samples$empirical_var - samples$true_var) / samples$true_var)


