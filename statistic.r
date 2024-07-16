calculate_c <- function(theta_0, theta_1, r, n, m, alpha, e) {
  c <- 0
  if (r > 0) {
    repeat {
      d <- 1 / (theta_1 ^ r - theta_0 ^ r) * log((c + e) * (theta_1 / theta_0) ^ (n * m))
      if (pgamma(d, shape = n * m / r, scale = 1 / theta_0 ^ r) > 1 - alpha) {
        break
      }
      c <- c + e
    }
  }
  else {
    repeat {
      d <- 1 / (theta_1 ^ r - theta_0 ^ r) * log((c + e) * (theta_1 / theta_0) ^ (n * m))
      #print(pgamma(d, shape = n * m / r, scale = 1 / theta_0 ^ r))
      if (pgamma(d, shape = n * m / r, scale = 1 / theta_0 ^ r) > alpha) {
        break
      }
      c <- c + e
    }
  }
  c
}

critical_value <- function(theta_0, theta_1, r, n, m, alpha, e) {
  c <- calculate_c(theta_0, theta_1, r, n, m, alpha, e)
  1 / (theta_1 ^ r - theta_0 ^ r) * log(c * (theta_1 / theta_0) ^ (n * m))
}

is_in_rejection_region <- function(d, theta_0, theta_1, r, n, m, alpha, e = 1e-2) {
  #print(critical_value(theta_0, theta_1, r, n, m, alpha, e))
  if (r > 0) {
    d >= critical_value(theta_0, theta_1, r, n, m, alpha, e)
  }
  else {
    d <= critical_value(theta_0, theta_1, r, n, m, alpha, e)
  }
}