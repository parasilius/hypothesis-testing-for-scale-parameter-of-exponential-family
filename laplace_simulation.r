source("statistic.r")

library("ExtDist")

n <- 20
r <- -1
m <- -1
alpha <- .05
theta_0 <- 8
theta_1 <- 3
replicates <- 10000
p <- numeric(replicates)
for (i in 1:replicates) {
  x <- VGAM::rlaplace(n, location = 0, scale = theta_0)
  d <- sum(abs(x))
  p[i] <- is_in_rejection_region(d, theta_0, theta_1, r, n, m, alpha)
}
p_hat <- mean(p)
se_hat <- sqrt(p_hat * (1 - p_hat) / replicates)
print(c(p_hat, se_hat))