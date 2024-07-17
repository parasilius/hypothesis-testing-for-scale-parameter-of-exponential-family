calculate_c <- function(theta_0, theta_1, r, n, m, alpha, e) {
  target <- if (r > 0) 1 - alpha else alpha
  c <- 0
  repeat {
    #print(c)
    x_ <- 1 / (theta_1 ^ r - theta_0 ^ r) * log((c + e) * (theta_1 / theta_0) ^ (n * m))
    ##print(pgamma(x_, shape = n * m / r, scale = 1 / theta_0 ^ r))
    #if (pgamma(x_, shape = n * m / r, scale = 1 / theta_0 ^ r) > target || c > 1) {
    if (pgamma(x_, shape = n * m / r, scale = 1 / theta_0 ^ r) > target) {
      break
    }
    c <- c + e
  }
  print(c)
  c
}

critical_value <- function(theta_0, theta_1, r, n, m, alpha, e = 1e-4) {
  c <- calculate_c(theta_0, theta_1, r, n, m, alpha, e)
  1 / (theta_1 ^ r - theta_0 ^ r) * log(c * (theta_1 / theta_0) ^ (n * m))
}

is_in_rejection_region <- function(d, cv, r) {
  #print(pgamma(d, shape = n * m / r, scale = 1 / theta_0 ^ r))
  #print(critical_value(theta_0, theta_1, r, n, m, alpha, e))
  if (r > 0) {
    d >= cv
  } else {
    d <= cv
  }
}