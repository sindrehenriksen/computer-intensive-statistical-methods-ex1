## ---- c
# Constant c
c_func = function(alpha) {
  exp(1) * alpha / (exp(1) + alpha)
}

## ---- g
# PDF g
g = function(x, alpha) {
  stopifnot(0 < alpha && alpha < 1)
  c = c_func(alpha)
  x = ifelse(0 < x,
             ifelse(x < 1, c * x^(alpha - 1), c * exp(-x)),
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
  x = ifelse((u < c / alpha), (u * alpha / c)^(1 / alpha),
             log(c / (1 - u)))
  return(x)
}

## ---- r_boxmuller
# Simulate n values from the standard normal distribution using the box muller algorithm
r_boxmuller <- function(n){
  x1 = 2*pi*runif(n)
  x2 = -2*log(runif(n))
  sample_box <-data.frame(y1 = sqrt(x2)*cos(x1), y2 = sqrt(x2)*cos(x1), x1 = x1, x2 = x2)
  return(sample_box)
}

## ---- k
# k is 1/acceptance probability
k = function(alpha) {
  (exp(1) + alpha) / (gamma(alpha + 1) * exp(1))
}

## ---- f
# PDF f
f_gamma1 = function(x, alpha) {
  ifelse(x <= 0, 0, x^(alpha - 1) * exp(-x) / gamma(alpha))
}

## ---- rf1
# Simulate n values from f with 0 < alpha < 1
r_gamma1 = function(n, alpha) {
  stopifnot(0 < alpha && alpha < 1)
  k = k(alpha)
  xs = numeric(n)
  n_accepted = 0
  while(n_accepted < n) {
    x = rg(1, alpha)
    acceptance_level = f_gamma1(x, alpha) / (g(x, alpha) * k)
    u = runif(1)
    if(u <= acceptance_level) {
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

## ---- rf2
# Simulate n values from f with alpha > 1
r_gamma2 = function(n, alpha) {
  stopifnot(alpha > 1)
  xs = numeric(n)
  n_accepted = 0
  n_tries = 0
  while(n_accepted < n){
    n_missing = n - n_accepted
    u1 = runif(n_missing)
    u2 = runif(n_missing)
    log_x1 = (alpha - 1) / 2 * log((alpha - 1) / exp(1)) + log(u1)
    log_x2 = (alpha + 1) / 2 * log((alpha + 1) / exp(1)) + log(u2)
    inside = log_x1 <= log_sqrt_f_star(log_x1, log_x2, alpha)
    n_inside = sum(inside)
    if(n_inside > 0) {
      xs[(n_accepted + 1):(n_accepted + n_inside)] = exp(log_x2[inside] -
                                                           log_x1[inside])
      n_accepted = n_accepted + n_inside
    }
    n_tries = n_tries + n_missing
  }
  return(list("x" = xs, "n_tries" = n_tries))
}

## ---- rf
# Simulate n values from a gamma distribution
r_gamma = function(n, alpha, beta) {
  stopifnot(alpha > 0 && beta > 0)
  if(alpha < 1) {
    x = r_gamma1(n, alpha)
  }
  else if(alpha == 1) {
    x = rexp(-1)  # !!!!!!!!!!
  }
  else {
    x = r_gamma2(n, alpha)$x
  }
  return(x / beta)
}
