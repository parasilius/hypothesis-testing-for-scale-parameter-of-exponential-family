source("statistic.r")

library("ExtDist")

n <- 20
r <- -1
m <- -1.5
alpha <- .05
theta_0 <- 8
theta_1 <- 3
replicates <- 1000
cv <- critical_value(theta_0, theta_1, r, n, m, alpha)
p <- numeric(replicates)
for (i in 1:replicates) {
  x <- VGAM::rmaxwell(n, 2 / theta_0)
  #print(x)
  d <- sum(x ^ 2)
  p[i] <- is_in_rejection_region(d, cv, r)
}
p_hat <- mean(p)
se_hat <- sqrt(p_hat * (1 - p_hat) / replicates)
print(c(p_hat, se_hat))