calculate_c <- function(theta_0, theta_1, r, n, m, alpha) {
  if (r > 0) {
    q <- qgamma(1 - alpha, shape = n * m / r, scale = 1 / theta_0 ^ r)
  } else {
    q <- qgamma(alpha, shape = n * m / r, scale = 1 / theta_0 ^ r)
  }
  (theta_0 / theta_1) ^ (n * m) * exp((theta_1 ^ r - theta_0 ^ r) * q)
}

critical_value <- function(theta_0, theta_1, r, n, m, alpha) {
  c <- calculate_c(theta_0, theta_1, r, n, m, alpha)
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