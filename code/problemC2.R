#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")

## ---- C2
set.seed(123)

# Params
K = 5
n = 100000
# Generating alphas from a uniform distribution U[0,20]
alpha = runif(K)*20
# z_k ~ Gamma(alpha_k, 1) so setting beta = 1
beta = 1

# Simulate from the dirichlet distribution
samples = r_dirichlet(K,n,alpha,beta)
# Calculate the maximum relative error in mean and covariance.
max((samples$empirical_mean - samples$true_mean) / samples$true_mean)
max((samples$empirical_cov - samples$true_cov) / samples$true_cov)