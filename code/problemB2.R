#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
source("functions.R")

## ---- B1
# Params
alpha = 10
n = 10000

# Simulate and calculate empirical and true means and variances
sim = rf3(n, alpha)
n_tries = sim$n_tries
samples = tibble(x = sim$x)
empirical_mean = mean(samples$x)
empirical_var = var(samples$x)
true_mean = true_var = alpha

# Plot histogram of distribution together with PDF
ggplot(data=samples) +
  geom_histogram(aes(x=x, y=..density..), bins=100, colour="white",
                 fill="cornflowerblue") +
  stat_function(fun=function(x)dgamma(x, shape=alpha), colour="darkred")
