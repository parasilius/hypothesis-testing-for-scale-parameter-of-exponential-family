source("statistic.r")

# defining parameters
n <- 20
r <- 1
m <- 1
alpha <- .05
theta_0 <- 8
theta_1 <- 3

# calculating critical value
cv <- critical_value(theta_0, theta_1, r, n, m, alpha)

# performing NHST
replicates <- 10000
p <- numeric(replicates)
for (i in 1:replicates) {
  x <- rbeta(n, shape1 = theta_0, shape2 = 1)
  d <- -1 * sum(log(x))
  p[i] <- is_in_rejection_region(d, cv, r)
}
p_hat <- mean(p)
se_hat <- sqrt(p_hat * (1 - p_hat) / replicates)

# caculating power function
n2 <- 1000
power <- 0
for (i in 1:n2) {
  x <- rbeta(n, shape1 = theta_1, shape2 = 1)
  d <- -1 * sum(log(x))
  power <- if (is_in_rejection_region(d, cv, r)) power + 1 else power
}
power <- power / n2

print(c(p_hat, se_hat, power))