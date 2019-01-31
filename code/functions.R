## ---- c
# Constant c
c_func = function(alpha) {
  exp(1) * alpha / (exp(1) + alpha)
}

## ---- r_exp
# Simulate n values from Exp(1)
r_exp <- function(n, lambda) {
  exp_dist = -1 / lambda * log(runif(n))
  return(exp_dist)
}

## ---- g
# PDF g
g = function(x, alpha) {
  stopifnot(0 < alpha && alpha < 1)
  c = c_func(alpha)
  x = ifelse(0 < x,
             ifelse(x < 1, c * x ^ (alpha - 1), c * exp(-x)),
             0)
  return(x)
}

## ---- rg
# Simulate n values from g with 0 < alpha < 1
rg = function(n, alpha) {
  stopifnot(0 < alpha && alpha < 1)
  c = c_func(alpha)
  u = runif(n)
  x = numeric(n)
  x = ifelse((u < c / alpha), (u * alpha / c) ^ (1 / alpha),
             log(c / (1 - u)))
  return(x)
}

## ---- r_boxmuller
# Simulate n values from the standard normal distribution using the box muller
# algorithm
r_boxmuller = function(n) {
  x1 = 2 * pi * runif(n)
  x2 = -2 * log(runif(n))
  sample_box = data.frame(
    y1 = sqrt(x2) * cos(x1),
    y2 = sqrt(x2) * cos(x1),
    x1 = x1,
    x2 = x2
  )
  return(sample_box)
}

## ---- r_multinorm
# Simulate d x n values from a d-variate normal distribution
r_multinorm = function(n, d) {
  x = matrix(0, d, n)
  for (i in 1:d) {
    df = r_boxmuller(n)
    x[i, ] = t(df[, 1])
    if ((d - i) > 0) {
      i = i + 1
      x[i, ] = t(df[, 2])
    }
  }
  A = matrix(0, d, d)
  for (i in 1:d) {
    A[, i] = runif(d)
  }
  mu = runif(d) * 10
  multinorm = list(y = mu + A %*% x,
                   true_mean = mu,
                   true_var = A %*% t(A))
  return(multinorm)
}

## ---- k
# k is 1/acceptance probability
k = function(alpha) {
  (exp(1) + alpha) / (gamma(alpha + 1) * exp(1))
}

## ---- f_gamma1
# PDF f
f_gamma1 = function(x, alpha) {
  ifelse(x <= 0, 0, x ^ (alpha - 1) * exp(-x) / gamma(alpha))
}

## ---- r_gamma1
# Simulate n values from f with 0 < alpha < 1
r_gamma1 = function(n, alpha) {
  stopifnot(0 < alpha && alpha < 1)
  k = k(alpha)
  xs = numeric(n)
  n_accepted = 0
  while (n_accepted < n) {
    x = rg(1, alpha)
    acceptance_level = f_gamma1(x, alpha) / (g(x, alpha) * k)
    u = runif(1)
    if (u <= acceptance_level) {
      n_accepted = n_accepted + 1
      xs[n_accepted] = x
    }
  }
  return(xs)
}

## ---- log_sqrt_f_star
# Function sqrt(f*(x2/x1)), logarithmic implementation
log_sqrt_f_star = function(log_x1, log_x2, alpha) {
  0.5 * ((alpha - 1) * (log_x2 - log_x1) - exp(log_x2 - log_x1))
}

## ---- r_gamma2
# Simulate n values from f with alpha > 1
r_gamma2 = function(n, alpha) {
  stopifnot(alpha > 1)
  xs = numeric(n)
  n_accepted = 0
  n_tries = 0
  while (n_accepted < n) {
    n_missing = n - n_accepted
    u1 = runif(n_missing)
    u2 = runif(n_missing)
    log_x1 = (alpha - 1) / 2 * log((alpha - 1) / exp(1)) + log(u1)
    log_x2 = (alpha + 1) / 2 * log((alpha + 1) / exp(1)) + log(u2)
    inside = log_x1 <= log_sqrt_f_star(log_x1, log_x2, alpha)
    n_inside = sum(inside)
    if (n_inside > 0) {
      xs[(n_accepted + 1):(n_accepted + n_inside)] = exp(log_x2[inside] -
                                                           log_x1[inside])
      n_accepted = n_accepted + n_inside
    }
    n_tries = n_tries + n_missing
  }
  return(list("x" = xs, "n_tries" = n_tries))
}

## ---- r_gamma
# Simulate n values from a gamma distribution
r_gamma = function(n, alpha, beta) {
  stopifnot(alpha > 0 && beta > 0)
  if (alpha < 1) {
    x = r_gamma1(n, alpha)
  }
  else if (alpha == 1) {
    x = rexp(-1)  # !!!!!!!!!!
  }
  else {
    x = r_gamma2(n, alpha)$x
  }
  return(x / beta)
}

## ---- r_dirichlet
# Simulate n x (K-1) values from a dirichlet distribution
r_dirichlet <- function(K, n, alpha, beta) {
  x = matrix(0, n, K - 1)
  z = matrix(0, n, K)
  for (k in 1:K) {
    z[, k] = r_gamma(n, alpha[k], beta)
  }
  x = z[, 1:(K - 1)] / rowSums(z)
  A = sum(alpha)
  true_var = matrix(0, K - 1, K - 1)
  for (i in 1:(K - 1)) {
    for (j in i:(K - 1)) {
      if (i != j) {
        true_var[i, j] = -alpha[i] * alpha[j] / (A ^ 2 * (A + 1))
        true_var[j, i] = true_var[i, j]
      } else{
        true_var[i, j] = alpha[i] / (A * (A + 1)) - alpha[i] ^ 2 /
          (A ^ 2 * (A + 1))
      }
    }
  }
  return(
    list(
      x = x,
      empirical_mean = colMeans(x),
      empirical_var = var(x),
      true_mean = alpha[1:(K - 1)] / A,
      true_var = true_var
    )
  )
}

  ## ---- f_star_d
  # Posterior density f(theta|y) (up to a normalising constant)
  f_posterior_star = function(theta, y) {
    stopifnot(0 < theta && theta < 1)
    stopifnot(length(y) == 4)
    return((2 + theta) ^ y[1] * (1 - theta) ^ (y[2] + y[3]) * theta ^ y[4])
  }

  ## ---- r_d
  # Simulate n values from f with 0 < alpha < 1
  r_posterior = function(n, y) {
    f_star_max = -optim(0.5, function(x)
      (-f_posterior_star(x, y)),
      method = "L-BFGS-B", lower = 1e-10, upper = 1 - 1e-10)$value
    c = integrate(function(x)
      (f_posterior_star(x, y)),
      lower = 0,
      upper = 1)$value
    f_posterior = function(x)
      (f_posterior_star(x, y) / c)
    k = f_star_max / c  # 1 / acceptance probability
    xs = numeric(n)
    n_accepted = 0
    n_random_numbers = 0
    while (n_accepted < n) {
      n_missing = n - n_accepted
      x = runif(n_missing)
      acceptance_level = f_posterior(x) / k
      u = runif(n_missing)
      inside = u <= acceptance_level
      n_inside = sum(inside)
      if (n_inside > 0) {
        xs[(n_accepted + 1):(n_accepted + n_inside)] = x[inside]
        n_accepted = n_accepted + n_inside
      }
      n_random_numbers = n_random_numbers + n_missing
    }
    return(list("x" = xs, "n_random_numbers" = n_random_numbers))
  }

  r_posterior_approx = function(n, y) {
    m = 20 * n
    u = runif(m)
    f_over_g = f_posterior_star(u, y) / 1
    weights = f_over_g / sum(f_over_g)
    x = sample(u, size = n, prob = weights)
    return(list(x = x, n_random_numbers = m))
  }

  ## ---- f_star_5_d
  # Posterior density f(theta|y) with prior Beta(1,5)
  # (up to a normalising constant)
  f_posterior_5_star = function(theta, y) {
    stopifnot(0 < theta && theta < 1)
    stopifnot(length(y) == 4)
    return((2 + theta) ^ y[1] * (1 - theta) ^ (y[2] + y[3] + 4) * theta ^
             y[4])
  }

  ## ---- is_d
  # Use importance sampling to estimate the posterior mean with prior Beta(1,5)
  posterior_mean_is = function(n, y) {
    u = runif(n)
    weights = f_posterior_5_star(u, y)
    mean_is = sum(u * weights) / sum(weights)
    return(mean_is)
  }
