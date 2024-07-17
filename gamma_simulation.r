source("statistic.r")

# defining parameters
n <- 20
r <- -1
beta <- 2
m <- -1 * beta
alpha <- .05
theta_0 <- 8
theta_1 <- 3

# calculating critical value
cv <- critical_value(theta_0, theta_1, r, n, m, alpha)

# performing NHST
replicates <- 10000
p <- numeric(replicates)
for (i in 1:replicates) {
  x <- rgamma(n, beta, scale = theta_0)
  d <- sum(x)
  p[i] <- is_in_rejection_region(d, cv, r)
}
p_hat <- mean(p)
se_hat <- sqrt(p_hat * (1 - p_hat) / replicates)

# caculating power function
n2 <- 1000
power <- 0
for (i in 1:n2) {
  x <- rgamma(n, beta, scale = theta_1)
  d <- sum(x)
  power <- if (is_in_rejection_region(d, cv, r)) power + 1 else power
}
power <- power / n2

print(c(p_hat, se_hat, power))