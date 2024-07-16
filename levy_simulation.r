source("statistic.r")

library("rmutil")

n <- 20
r <- -1
m <- -0.5
alpha <- .05
theta_0 <- 8
theta_1 <- 3
replicates <- 10000
p <- numeric(replicates)
for (i in 1:replicates) {
  x <- rmutil::rlevy(n, m = 0, s = 1 / theta_0)
  d <- 0.5 * sum(1 / x)
  p[i] <- is_in_rejection_region(d, theta_0, theta_1, r, n, m, alpha, 1e-300)
}
p_hat <- mean(p)
se_hat <- sqrt(p_hat * (1 - p_hat) / replicates)
print(c(p_hat, se_hat))