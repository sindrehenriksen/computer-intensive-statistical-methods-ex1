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

## ---- k
# k is 1/acceptance probability
k = function(alpha) {
  (exp(1) + alpha) / (gamma(alpha + 1) * exp(1))
}

## ---- f
# PDF f
f = function(x, alpha) {
  ifelse(x <= 0, 0, x^(alpha - 1) * exp(-x) / gamma(alpha))
}

## ---- rf1
# Simulate n values from f with 0 < alpha < 1
rf1 = function(n, alpha) {
  stopifnot(0 < alpha && alpha < 1)
  k = k(alpha)
  xs = numeric(n)
  n_accepted = 0
  while(n_accepted < n) {
    x = rg(1, alpha)
    acceptance_level = f(x, alpha) / (g(x, alpha) * k)
    u = runif(1)
    if(u <= acceptance_level) {
      n_accepted = n_accepted + 1
      xs[n_accepted] = x
    }
  }
  return(xs)
}

## ---- f_star
# Function f*(x)
f_star = function(x, alpha) {
  ifelse(x > 0, x^(alpha - 1) * exp(-x), 0)
}

## ---- f_star_log
# Function f*(x2/x1), logarithmic implementation
sqrt_f_star_log = function(x1, x2, alpha) {
  exp(0.5 * ((alpha - 1) * (log(x2) - log(x1)) - x2/x1))
}

## ---- rf2
# Simulate n values from f with alpha > 1
rf2 = function(n, alpha) {
  stopifnot(alpha > 1)
  a = sqrt(alpha - 1)
  b = sqrt(alpha + 1)  # b_plus, b_minus = 0
  xs = numeric(n)
  saved_u1 = numeric(n)
  saved_u2 = numeric(n)
  n_accepted = 0
  n_tries = 0
  while(n_accepted < n){
    n_missing = n - n_accepted
    u1 = runif(n_missing, 0, a)
    u2 = runif(n_missing, 0, b)
    inside = u1 <= sqrt_f_star_log(u1, u2, alpha)
    n_inside = sum(inside)
    if(n_inside > 0) {
      saved_u2[(n_accepted + 1):(n_accepted + n_inside)] = u2[inside]
      saved_u1[(n_accepted + 1):(n_accepted + n_inside)] = u1[inside]
      xs[(n_accepted + 1):(n_accepted + n_inside)] = u2[inside] / u1[inside]
      n_accepted = n_accepted + n_inside
    }
    n_tries = n_tries + n_missing
  }
  return(list("x" = xs, "n_tries" = n_tries, "u2"=saved_u2, "u1"=saved_u1))
}

rf3 = function(n, alpha) {
  stopifnot(alpha > 1)
  a = sqrt(alpha - 1)
  b = sqrt(alpha + 1)  # b_plus, b_minus = 0
  xs = numeric(n)
  n_accepted = 0
  n_tries = 0
  while(n_accepted < n){
    u1 = runif(1, 0, a)
    u2 = runif(1, 0, b)
    if(u1 <= sqrt_f_star_log(u1, u2, alpha)) {
      xs[n_accepted] = u2 / u1
      n_accepted = n_accepted + 1
    }
    n_tries = n_tries + 1
  }
  return(list("x" = xs, "n_tries" = n_tries))
}

## ---- rf
# Simulate n values from a gamma distribution
rf = function(n, alpha, beta) {
  stopifnot(alpha > 0 && beta > 0)
  # x = ifelse(alpha > 1, rf2(n, alpha)$x, rf1(n, alpha))
  x = rgamma(n, shape=alpha)
  return(x / beta)
}
