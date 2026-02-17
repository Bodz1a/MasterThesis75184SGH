# Simulations

## 1. Consumer population


clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

make_population <- function(Ncons,
                            shape_R = 4, scale_R = 1,
                            a_S = 4, b_S = 4,
                            seed = 1) {
  set.seed(seed)
  data.frame(
    R = rgamma(Ncons, shape = shape_R, scale = scale_R),
    S = rbeta(Ncons, shape1 = a_S, shape2 = b_S)
  )
}

utility_single <- function(R, S, p, o, alpha, beta) {
  -alpha * (p / R) - beta * (S - o)^2
}

logit <- function(U) 1 / (1 + exp(-U))

# expected demand and profit for 1 firm
profit_1firm <- function(agents, p, o, alpha, beta, c_mc) {
  U <- utility_single(agents$R, agents$S, p, o, alpha, beta)
  P <- logit(U)
  Qhat <- sum(P)
  pi_hat <- (p - c_mc) * Qhat
  list(Q = Qhat, profit = pi_hat)
}


## 2. Single-firm learning algorithm


learn_1firm <- function(agents,
                        p0, o0,
                        alpha, beta, c_mc,
                        dp = 0.2, do = 0.02,
                        p_min = 0, p_max = 20,
                        o_min = 0, o_max = 1,
                        T = 5000,
                        eps_p = 1e-4, eps_o = 1e-4,
                        patience = 50) {
  
  p <- clip(p0, p_min, p_max)
  o <- clip(o0, o_min, o_max)
  
  hist <- data.frame(t = integer(0), p = numeric(0), o = numeric(0),
                     Q = numeric(0), profit = numeric(0))
  
  stable_count <- 0
  
  for (t in 1:T) {
    
    cur <- profit_1firm(agents, p, o, alpha, beta, c_mc)
    
    hist <- rbind(hist, data.frame(t = t, p = p, o = o,
                                   Q = cur$Q, profit = cur$profit))
    if (t %% 2 == 1) {
      cand <- data.frame(
        p = clip(c(p - dp, p, p + dp), p_min, p_max),
        o = o
      )
    } else {
      cand <- data.frame(
        p = p,
        o = clip(c(o - do, o, o + do), o_min, o_max)
      )
    }
    
    # evaluate candidates deterministically
    vals <- apply(cand, 1, function(x) profit_1firm(agents, x[1], x[2], alpha, beta, c_mc)$profit)
    best_i <- which.max(vals)
    
    p_new <- cand$p[best_i]
    o_new <- cand$o[best_i]
    
    # convergence check
    if (abs(p_new - p) < eps_p && abs(o_new - o) < eps_o) {
      stable_count <- stable_count + 1
    } else {
      stable_count <- 0
    }
    
    p <- p_new; o <- o_new
    
    if (stable_count >= patience) break
  }
  
  final <- profit_1firm(agents, p, o, alpha, beta, c_mc)
  list(p_star = p, o_star = o,
       profit_star = final$profit, Q_star = final$Q,
       iters = nrow(hist), history = hist)
}


## 3. Two-firm demand and profit


utility_firm <- function(R, S, p, o, alpha, beta) {
  -alpha * (p / R) - beta * (S - o)^2
}

# deterministic shares and profits
profit_2firm <- function(agents,
                         p1, o1, p2, o2,
                         alpha, beta, c1, c2) {
  
  u1 <- utility_firm(agents$R, agents$S, p1, o1, alpha, beta)
  u2 <- utility_firm(agents$R, agents$S, p2, o2, alpha, beta)
  
  e1 <- exp(u1)
  e2 <- exp(u2)
  denom <- 1 + e1 + e2
  
  P1 <- e1 / denom
  P2 <- e2 / denom
  P0 <- 1 / denom
  
  Q1 <- sum(P1)
  Q2 <- sum(P2)
  s0 <- mean(P0)
  
  pi1 <- (p1 - c1) * Q1
  pi2 <- (p2 - c2) * Q2
  
  list(Q1 = Q1, Q2 = Q2, s0 = s0, pi1 = pi1, pi2 = pi2)
}


## 4. Two-firm learning


learn_2firm <- function(agents,
                        p10, o10, p20, o20,
                        alpha, beta, c1, c2,
                        dp = 0.2, do = 0.02,
                        p_min = 0, p_max = 20,
                        o_min = 0, o_max = 1,
                        T = 5000,
                        eps = 1e-4,
                        patience = 50) {
  
  p1 <- clip(p10, p_min, p_max); o1 <- clip(o10, o_min, o_max)
  p2 <- clip(p20, p_min, p_max); o2 <- clip(o20, o_min, o_max)
  
  hist <- data.frame(t = integer(0),
                     p1 = numeric(0), o1 = numeric(0),
                     p2 = numeric(0), o2 = numeric(0),
                     Q1 = numeric(0), Q2 = numeric(0),
                     pi1 = numeric(0), pi2 = numeric(0),
                     s0 = numeric(0))
  
  stable_count <- 0
  
  for (t in 1:T) {
    cur <- profit_2firm(agents, p1, o1, p2, o2, alpha, beta, c1, c2)
    hist <- rbind(hist, data.frame(t = t,
                                   p1 = p1, o1 = o1, p2 = p2, o2 = o2,
                                   Q1 = cur$Q1, Q2 = cur$Q2,
                                   pi1 = cur$pi1, pi2 = cur$pi2, s0 = cur$s0))
    
    #Firm 1 updates (given firm 2 fixed)
    if (t %% 2 == 1) {
      cand1 <- data.frame(p1 = clip(c(p1 - dp, p1, p1 + dp), p_min, p_max),
                          o1 = o1)
    } else {
      cand1 <- data.frame(p1 = p1,
                          o1 = clip(c(o1 - do, o1, o1 + do), o_min, o_max))
    }
    vals1 <- apply(cand1, 1, function(x) profit_2firm(agents, x[1], x[2], p2, o2, alpha, beta, c1, c2)$pi1)
    b1 <- which.max(vals1)
    p1_new <- cand1$p1[b1]; o1_new <- cand1$o1[b1]
    
    #Firm 2 updates (given firm 1 updated)
    if (t %% 2 == 1) {
      cand2 <- data.frame(p2 = clip(c(p2 - dp, p2, p2 + dp), p_min, p_max),
                          o2 = o2)
    } else {
      cand2 <- data.frame(p2 = p2,
                          o2 = clip(c(o2 - do, o2, o2 + do), o_min, o_max))
    }
    vals2 <- apply(cand2, 1, function(x) profit_2firm(agents, p1_new, o1_new, x[1], x[2], alpha, beta, c1, c2)$pi2)
    b2 <- which.max(vals2)
    p2_new <- cand2$p2[b2]; o2_new <- cand2$o2[b2]
    
    # convergence check
    delta <- max(abs(p1_new - p1), abs(o1_new - o1), abs(p2_new - p2), abs(o2_new - o2))
    if (delta < eps) stable_count <- stable_count + 1 else stable_count <- 0
    
    p1 <- p1_new; o1 <- o1_new
    p2 <- p2_new; o2 <- o2_new
    
    if (stable_count >= patience) break
  }
  
  final <- profit_2firm(agents, p1, o1, p2, o2, alpha, beta, c1, c2)
  
  list(p1_star = p1, o1_star = o1, p2_star = p2, o2_star = o2,
       pi1_star = final$pi1, pi2_star = final$pi2,
       Q1_star = final$Q1, Q2_star = final$Q2, s0_star = final$s0,
       iters = nrow(hist), history = hist)
}


## 5. Experiment run


run_experiments_1firm <- function(agents, grid) {
  out <- lapply(1:nrow(grid), function(i) {
    g <- grid[i, ]
    res <- learn_1firm(agents,
                       p0 = g$p0, o0 = g$o0,
                       alpha = g$alpha, beta = g$beta, c_mc = g$c,
                       dp = g$dp, do = g$do,
                       p_max = g$pmax, T = g$T)
    data.frame(
      alpha = g$alpha, beta = g$beta, c = g$c,
      dp = g$dp, do = g$do, pmax = g$pmax,
      p0 = g$p0, o0 = g$o0,
      p_star = res$p_star, o_star = res$o_star,
      profit_star = res$profit_star, Q_star = res$Q_star,
      iters = res$iters
    )
  })
  do.call(rbind, out)
}

run_experiments_2firm <- function(agents, grid) {
  out <- lapply(1:nrow(grid), function(i) {
    g <- grid[i, ]
    res <- learn_2firm(agents,
                       p10 = g$p10, o10 = g$o10, p20 = g$p20, o20 = g$o20,
                       alpha = g$alpha, beta = g$beta,
                       c1 = g$c1, c2 = g$c2,
                       dp = g$dp, do = g$do,
                       p_max = g$pmax, T = g$T)
    data.frame(
      alpha = g$alpha, beta = g$beta, c1 = g$c1, c2 = g$c2,
      dp = g$dp, do = g$do, pmax = g$pmax,
      p10 = g$p10, o10 = g$o10, p20 = g$p20, o20 = g$o20,
      p1_star = res$p1_star, o1_star = res$o1_star,
      p2_star = res$p2_star, o2_star = res$o2_star,
      pi1_star = res$pi1_star, pi2_star = res$pi2_star,
      Q1_star = res$Q1_star, Q2_star = res$Q2_star,
      s0_star = res$s0_star,
      diff_o = abs(res$o1_star - res$o2_star),
      iters = res$iters
    )
  })
  do.call(rbind, out)
}


## 6. Define scenarios and run them


# Create one population for all experiments (keeps comparisons clean)
agents <- make_population(Ncons = 5000, seed = 1)

#Single firm: vary initial conditions + key parameters
grid1 <- expand.grid(
  alpha = c(0.5, 1, 2),
  beta  = c(0.5, 1, 2),
  c     = c(0.2, 0.5, 1.0),
  dp    = c(0.2),
  do    = c(0.02),
  pmax  = c(20),
  T     = c(5000),
  p0    = c(1, 5, 10),
  o0    = c(0.2, 0.5, 0.8)
)

res1 <- run_experiments_1firm(agents, grid1)

# Inspect top outcomes
res1_sorted <- res1[order(-res1$profit_star), ]
print(head(res1_sorted, 10))

# Two firms: symmetric costs, different initial positions
grid2 <- expand.grid(
  alpha = c(0.5, 1, 2),
  beta  = c(0.5, 1, 2),
  c1    = c(0.5),
  c2    = c(0.5),
  dp    = c(0.2),
  do    = c(0.02),
  pmax  = c(20),
  T     = c(5000),
  p10   = c(2, 8),
  o10   = c(0.2, 0.5),
  p20   = c(2, 8),
  o20   = c(0.5, 0.8)
)

res2 <- run_experiments_2firm(agents, grid2)
res2_sorted <- res2[order(-(res2$pi1_star + res2$pi2_star)), ]
print(head(res2_sorted, 10))


## 7. Simple plots


par(mar = c(4,4,2,1))

## Single firm: how optimal price depends on alpha (holding others vary)
plot(res1$alpha, res1$p_star, pch = 16,
     xlab = "alpha (price sensitivity)",
     ylab = "learned p*",
     main = "Single firm: learned p* vs alpha")

## Two firms: differentiation vs beta
plot(res2$beta, res2$diff_o, pch = 16,
     xlab = "beta (taste mismatch sensitivity)",
     ylab = "|o1* - o2*|",
     main = "Duopoly: differentiation increases with beta (often)")

## Duopoly: total profit vs alpha
plot(res2$alpha, res2$pi1_star + res2$pi2_star, pch = 16,
     xlab = "alpha",
     ylab = "pi1* + pi2*",
     main = "Duopoly: total profit vs alpha")

####### EXPERIMENTS separate

agents <- make_population(Ncons = 5000, seed = 1)

# Experiment 1

E1 <- data.frame(alpha = c(0.5, 1, 2, 3))
E1$beta <- 1
E1$c <- 0.5
E1$p0 <- 5
E1$o0 <- 0.5

resE1 <- do.call(rbind, lapply(1:nrow(E1), function(i){
  g <- E1[i,]
  r <- learn_1firm(agents,
                   p0=g$p0, o0=g$o0,
                   alpha=g$alpha, beta=g$beta, c_mc=g$c,
                   dp=0.2, do=0.02,
                   p_max=20, T=5000)
  data.frame(alpha=g$alpha, beta=g$beta, c=g$c,
             p_star=r$p_star, o_star=r$o_star,
             profit_star=r$profit_star, Q_star=r$Q_star,
             iters=r$iters)
}))
print(resE1)

setwd("~/Master Thesis/MasterThesis75184SGH/paper")
png("fig_alpha_single.png", width=1200, height=900, res=300)
par(mar=c(4,4,2,1))

plot(resE1$alpha, resE1$p_star, pch=16,
     xlab="alpha (price sensitivity)", ylab="learned p*",
    # main="E1: Single firm — optimal price vs alpha")
    )
dev.off()
# Experiment 2

betas <- c(0.2, 0.5, 1, 2, 4)
o0s <- c(0.2, 0.8)

E2 <- expand.grid(beta = betas, o0 = o0s)
E2$alpha <- 1
E2$c <- 0.5
E2$p0 <- 5

resE2 <- do.call(rbind, lapply(1:nrow(E2), function(i){
  g <- E2[i,]
  r <- learn_1firm(agents,
                   p0=g$p0, o0=g$o0,
                   alpha=g$alpha, beta=g$beta, c_mc=g$c,
                   dp=0.2, do=0.02,
                   p_max=20, T=5000)
  data.frame(alpha=g$alpha, beta=g$beta, c=g$c, o0=g$o0,
             p_star=r$p_star, o_star=r$o_star,
             profit_star=r$profit_star, Q_star=r$Q_star)
}))
print(resE2)


par(mar=c(4,4,2,1))
plot(resE2$beta, resE2$o_star, pch=16,
     xlab="beta (taste mismatch sensitivity)", ylab="learned o*",
     main="E2: Single firm — optimal style vs beta")
abline(h=0.5, lty=2)   # taste center for Beta(4,4)

# Experiment 3

mono <- learn_1firm(agents, p0=5, o0=0.5, alpha=1, beta=1, c_mc=0.5,
                    dp=0.2, do=0.02, p_max=20, T=5000)
cat("Monopoly p* =", mono$p_star, " o* =", mono$o_star, " profit =", mono$profit_star, "\n")

duo <- learn_2firm(agents,
                   p10=5, o10=0.5,
                   p20=5, o20=0.5,
                   alpha=1, beta=1,
                   c1=0.5, c2=0.5,
                   dp=0.2, do=0.02, p_max=20, T=5000)
cat("Duopoly p1* =", duo$p1_star, " p2* =", duo$p2_star, "\n")
cat("Duopoly o1* =", duo$o1_star, " o2* =", duo$o2_star, "\n")
cat("Profits: pi1* =", duo$pi1_star, " pi2* =", duo$pi2_star, " total =", duo$pi1_star+duo$pi2_star, "\n")
cat("Outside share s0* =", duo$s0_star, "\n")

# Experiment 4

betas <- c(0.2, 0.5, 1, 2, 4)

resE4 <- do.call(rbind, lapply(betas, function(b){
  r <- learn_2firm(agents,
                   p10=5, o10=0.2,
                   p20=5, o20=0.8,
                   alpha=1, beta=b,
                   c1=0.5, c2=0.5,
                   dp=0.2, do=0.02, p_max=20, T=5000)
  data.frame(beta=b,
             o1_star=r$o1_star, o2_star=r$o2_star,
             diff_o=abs(r$o1_star-r$o2_star),
             p1_star=r$p1_star, p2_star=r$p2_star,
             total_profit=r$pi1_star+r$pi2_star,
             s0_star=r$s0_star)
}))
print(resE4)

par(mar=c(4,4,2,1))
plot(resE4$beta, resE4$diff_o, pch=16,
     xlab="beta", ylab="|o1* - o2*|",
     main="E4: Duopoly — differentiation vs beta")

# Experiment 5 (change distribution)

taste_cases <- data.frame(
  a_S = c(2, 4, 5),
  b_S = c(5, 4, 2),
  label = c("Left-skew Beta(2,5)", "Symmetric Beta(4,4)", "Right-skew Beta(5,2)")
)

resTaste1 <- do.call(rbind, lapply(1:nrow(taste_cases), function(i){
  tc <- taste_cases[i,]
  
  agents_tc <- make_population(Ncons = 5000, a_S = tc$a_S, b_S = tc$b_S, seed = 1)
  
  r <- learn_1firm(agents_tc,
                   p0 = 5, o0 = 0.5,
                   alpha = 1, beta = 1, c_mc = 0.5,
                   dp = 0.2, do = 0.02,
                   p_max = 20, T = 5000)
  
  data.frame(
    case = tc$label,
    a_S = tc$a_S, b_S = tc$b_S,
    mean_S = tc$a_S / (tc$a_S + tc$b_S),
    p_star = r$p_star,
    o_star = r$o_star,
    profit_star = r$profit_star,
    Q_star = r$Q_star
  )
}))

print(resTaste1)

png("fig_taste_single.png", width=1200, height=900, res=300)
par(mar=c(4,4,2,1))
plot(resTaste1$mean_S, resTaste1$o_star, pch=16,
     xlab="Mean of taste distribution E[S]",
     ylab="Learned optimal style o*",
    # main="Single firm: o* tracks taste distribution asymmetry")
    )
abline(0,1,lty=2)
dev.off()

# Experiment 6

resTaste2 <- do.call(rbind, lapply(1:nrow(taste_cases), function(i){
  tc <- taste_cases[i,]
  agents_tc <- make_population(Ncons = 5000, a_S = tc$a_S, b_S = tc$b_S, seed = 1)
  
  r <- learn_2firm(agents_tc,
                   p10=5, o10=0.2,
                   p20=5, o20=0.8,
                   alpha=1, beta=1,
                   c1=0.5, c2=0.5,
                   dp=0.2, do=0.02, p_max=20, T=5000)
  
  data.frame(
    case = tc$label,
    mean_S = tc$a_S / (tc$a_S + tc$b_S),
    o1_star = r$o1_star, o2_star = r$o2_star,
    diff_o = abs(r$o1_star - r$o2_star),
    p1_star = r$p1_star, p2_star = r$p2_star,
    total_profit = r$pi1_star + r$pi2_star,
    s0_star = r$s0_star
  )
}))

print(resTaste2)

png("fig_taste_duopoly.png", width=1300, height=900, res=300)
par(mar=c(4,4,2,1))
plot(resTaste2$mean_S, resTaste2$o1_star, pch=16,
     xlab="E[S]", ylab="Learned o1* (firm 1)",
     abline(a=0, b=1, lty=2, col="black", lwd=2)
    # main="Duopoly: firm positions shift with taste distribution")
)
points(resTaste2$mean_S, resTaste2$o2_star, pch=16)
dev.off()

### PATHS

run1 <- learn_1firm(agents,
                    p0=5, o0=0.5,
                    alpha=1, beta=1, c_mc=0.5,
                    dp=0.01, do=0.01,
                    p_max=20, T=5000)

h1 <- run1$history

# Directory for figures (change if you want)
dir.create("figures", showWarnings = FALSE)

# (A) Price path
png("path_single_price.png", width=1300, height=900, res=300)
par(mar=c(4,4,1,1))
plot(h1$t, h1$p, type="l",
     xlab="Iteration t", ylab="Price p_t")
dev.off()

# (B) Style path
png("path_single_style.png", width=1300, height=900, res=300)
par(mar=c(4,4,1,1))
plot(h1$t, h1$o, type="l",
     xlab="Iteration t", ylab="Product characteristic o_t")
dev.off()

# (C) Profit path
png("path_single_profit.png", width=1300, height=900, res=300)
par(mar=c(4,4,1,1))
plot(h1$t, h1$profit, type="l",
     xlab="Iteration t", ylab="Expected profit Π_t")
dev.off()

### PATHS 2 FIRMS

run2 <- learn_2firm(agents,
                    p10=5, o10=0.2,
                    p20=5.2, o20=0.8,
                    alpha=1, beta=1,
                    c1=0.5, c2=0.5,
                    dp=0.01, do=0.01,
                    p_max=20, T=5000)

h2 <- run2$history

# (A) Prices path (both firms)
png("path_duopoly_prices.png", width=1300, height=900, res=300)
par(mar=c(4,4,1,1))
plot(h2$t, h2$p1, type="l",
     xlab="Iteration t", ylab="Price",
     ylim=range(c(h2$p1,h2$p2)))
lines(h2$t, h2$p2, type="l", lty=2)
legend("topright", legend=c("Firm 1", "Firm 2"), lty=c(1,2), bty="n")
dev.off()

# (B) Styles path (both firms)
png("path_duopoly_styles.png", width=1300, height=900, res=300)
par(mar=c(4,4,1,1))
plot(h2$t, h2$o1, type="l",
     xlab="Iteration t", ylab="Product characteristic",
     ylim=range(c(h2$o1,h2$o2)))
lines(h2$t, h2$o2, type="l", lty=2)
legend("topright", legend=c("Firm 1", "Firm 2"), lty=c(1,2), bty="n")
dev.off()

# (C) Profit paths (both firms)
png("path_duopoly_profits.png", width=1300, height=900, res=300)
par(mar=c(4,4,1,1))
plot(h2$t, h2$pi1, type="l",
     xlab="Iteration t", ylab="Expected profit",
     ylim=range(c(h2$pi1,h2$pi2)))
lines(h2$t, h2$pi2, type="l", lty=2)
legend("topright", legend=c("Firm 1", "Firm 2"), lty=c(1,2), bty="n")
dev.off()
