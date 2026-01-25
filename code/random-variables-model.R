simulate_market <- function(N, p1, p2, o1, o2,
                                 alpha = -1, beta = -1,
                                 c_cost = 1,
                                 shape_R = 4, scale_R = 1,
                                 a_S = 4, b_S = 4) {
  
  # random variables for consumer types
  R <- rgamma(N, shape = shape_R, scale = scale_R)
  R <- pmax(R, 1e-12)
  S <- rbeta(N, shape1 = a_S, shape2 = b_S)
  
  # logit indices
  z1 <- -alpha * (p1 / R) - beta * (S - o1)^2
  z2 <- -alpha * (p2 / R) - beta * (S - o2)^2
  
  # Stable softmax with outside option utility 0
  m  <- pmax(0, z1, z2)
  e0 <- exp(0  - m)
  e1 <- exp(z1 - m)
  e2 <- exp(z2 - m)
  den <- e0 + e1 + e2
  
  pr1 <- e1 / den
  pr2 <- e2 / den
  
  # random variables for choices Y_i
  u <- runif(N)
  Y <- ifelse(u < pr1, 1,
              ifelse(u < pr1 + pr2, 2, 0))
  
  #random variables for demands
  D1 <- sum(Y == 1)
  D2 <- sum(Y == 2)
  
  # profit random variables (quadratic cost)
  pi1 <- p1 * D1 - c_cost * (D1^2)
  pi2 <- p2 * D2 - c_cost * (D2^2)
  
  c(D1 = D1, D2 = D2, pi1 = pi1, pi2 = pi2)
}

#Monte Carlo estimate
simulate_mc <- function(M, N, p1, p2, o1, o2,
                        alpha = -1, beta = -1,
                        c_cost = 1,
                        shape_R = 4, scale_R = 1,
                        a_S = 4, b_S = 4) {
  D1 <- numeric(M); D2 <- numeric(M)
  pi1 <- numeric(M); pi2 <- numeric(M)
  
  for (m in 1:M) {
    out <- simulate_market_once(N, p1, p2, o1, o2,
                                alpha, beta, c_cost,
                                shape_R, scale_R, a_S, b_S)
    D1[m] <- out["D1"]; D2[m] <- out["D2"]
    pi1[m] <- out["pi1"]; pi2[m] <- out["pi2"]
  }
  
  list(
    E_D1 = mean(D1), E_D2 = mean(D2),
    E_pi1 = mean(pi1), E_pi2 = mean(pi2),
    D1 = D1, D2 = D2,
    pi1 = pi1, pi2 = pi2
  )
}

# two firms with fixed styles
N  <- 100
q1 <- 0.3
q2 <- 0.7

# prices
p1 <- 120
p2 <- 120

res <- simulate_mc(M = 2000, N = N, p1 = p1, p2 = p2, o1 = o1, o2 = o2,
                   alpha = -1, beta = -1, c_cost = 1)

res$E_pi1
res$E_pi2
