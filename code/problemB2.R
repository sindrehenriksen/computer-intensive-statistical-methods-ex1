#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
source("functions.R")

## ---- B2
set.seed(123)

# Params
alpha = 5
n = 10000

# Simulate and calculate empirical and true means and variances
sim = r_gamma2(n, alpha)
n_tries = sim$n_tries
print(n_tries)
samples = tibble(x = sim$x)
empirical_mean = mean(samples$x)
empirical_var = var(samples$x)
true_mean = true_var = alpha

# Plot histogram of distribution together with PDF
ggplot(data=samples) +
  geom_histogram(aes(x=x, y=..density..), bins=100, colour="white",
                 fill="cornflowerblue") +
  stat_function(fun=function(x)dgamma(x, shape=alpha), colour="darkred")

# Compare empirical means and variances with true values for different alphas
empirical_means = empirical_vars = numeric(4)
alphas = c(5, 100, 2000, 5000)
for(i in 1:4) {
  x = r_gamma2(n, alphas[i])$x
  empirical_means[i] = mean(x)
  empirical_vars[i] = var(x)
}
true_means = true_vars = alphas
max((empirical_means - true_means) / true_means)
max((empirical_vars - true_vars) / true_vars)
