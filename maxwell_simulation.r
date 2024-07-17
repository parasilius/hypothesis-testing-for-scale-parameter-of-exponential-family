source("statistic.r")

library("ExtDist")

# defining parameters
n <- 20
r <- -1
m <- -1.5
alpha <- .05
theta_0 <- 8
theta_1 <- 3

# calculating critical value
cv <- critical_value(theta_0, theta_1, r, n, m, alpha)

# performing NHST
replicates <- 1000
p <- numeric(replicates)
for (i in 1:replicates) {
  x <- VGAM::rmaxwell(n, 2 / theta_0)
  d <- sum(x ^ 2)
  p[i] <- is_in_rejection_region(d, cv, r)
}
p_hat <- mean(p)
se_hat <- sqrt(p_hat * (1 - p_hat) / replicates)

# caculating power function
n2 <- 1000
power <- 0
for (i in 1:n2) {
  x <- VGAM::rmaxwell(n, 2 / theta_1)
  d <- sum(x ^ 2)
  power <- if (is_in_rejection_region(d, cv, r)) power + 1 else power
}
power <- power / n2

print(c(p_hat, se_hat, power))