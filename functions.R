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
