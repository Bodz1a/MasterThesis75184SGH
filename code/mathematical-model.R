# Parameters
alpha <- 1
beta  <- 1
c_mc  <- 0.5

Ncons <- 5000        # size of population of agents
T     <- 10000       # number of iterations

p_min <- 0
p_max <- 20
o_min <- 0
o_max <- 1

#step sizes (how much firm changes p or o)
dp <- 0.2
do <- 0.02

#distributions
shape_R <- 4; scale_R <- 1  # Gamma(4,1)
a_S <- 4; b_S <- 4          # Beta(4,4)

#population of agents
set.seed(1)
agents <- data.frame(
  R = rgamma(Ncons, shape = shape_R, scale = scale_R),
  S = rbeta(Ncons, shape1 = a_S, shape2 = b_S)
)

#demand model pieces
utility_vec <- function(R, S, p, o, alpha, beta) {
  -alpha * (p / R) - beta * (S - o)^2
}

buy_prob_vec <- function(R, S, p, o, alpha, beta) {
  U <- utility_vec(R, S, p, o, alpha, beta)
  1 / (1 + exp(-U))
}

#given (p,o), agents decide and firm observes demand & profit
simulate_market <- function(agents, p, o, alpha, beta, c_mc) {
  pr <- buy_prob_vec(agents$R, agents$S, p, o, alpha, beta)
  buys <- rbinom(n = nrow(agents), size = 1, prob = pr)
  
  Q <- sum(buys)                 # observed demand
  profit <- (p - c_mc) * Q       # observed profit
  
  list(demand = Q, profit = profit)
}

#firm adjustment rule (adjust either price OR taste)
adjust_firm <- function(agents, p, o, dp, do,
                        p_min, p_max, o_min, o_max,
                        alpha, beta, c_mc,
                        adjust = c("price", "taste")) {
  
  adjust <- match.arg(adjust)
  
  #current outcome
  cur <- simulate_market(agents, p, o, alpha, beta, c_mc)
  
  if (adjust == "price") {
    candidates <- c(
      clip(p - dp, p_min, p_max),
      p,
      clip(p + dp, p_min, p_max)
    )
    
    res <- lapply(candidates, function(pp) simulate_market(agents, pp, o, alpha, beta, c_mc))
    profits <- sapply(res, `[[`, "profit")
    best_i <- which.max(profits)
    
    list(p_new = candidates[best_i], o_new = o, outcome = res[[best_i]])
    
  } else {
    candidates <- c(
      clip(o - do, o_min, o_max),
      o,
      clip(o + do, o_min, o_max)
    )
    
    res <- lapply(candidates, function(oo) simulate_market(agents, p, oo, alpha, beta, c_mc))
    profits <- sapply(res, `[[`, "profit")
    best_i <- which.max(profits)
    
    list(p_new = p, o_new = candidates[best_i], outcome = res[[best_i]])
  }
}

## ======================== ##
## algorithm implementation ##
## ======================== ##

#initial values
p <- 2
o <- 0.2

history <- data.frame(
  t = integer(0),
  p = numeric(0),
  o = numeric(0),
  demand = numeric(0),
  profit = numeric(0),
  action = character(0)
)

for (t in 1:T) {
  
  #agents decide, firm observes
  obs <- simulate_market(agents, p, o, alpha, beta, c_mc)
  
  history <- rbind(history, data.frame(
    t = t, p = p, o = o,
    demand = obs$demand,
    profit = obs$profit,
    action = "observe"
  ))
  
  #firm adjusts either price or taste (choose one each iteration)
  action <- if (t %% 2 == 1) "price" else "taste"
  
  upd <- adjust_firm(
    agents = agents, p = p, o = o,
    dp = dp, do = do,
    p_min = p_min, p_max = p_max,
    o_min = o_min, o_max = o_max,
    alpha = alpha, beta = beta, c_mc = c_mc,
    adjust = action
  )
  
  p <- upd$p_new
  o <- upd$o_new
  
  #record the post-adjustment outcome
  history <- rbind(history, data.frame(
    t = t, p = p, o = o,
    demand = upd$outcome$demand,
    profit = upd$outcome$profit,
    action = paste0("adjust_", action)
  ))
}

#final result
cat("Final parameters after learning:\n")
cat("p =", p, " o =", o, "\n")
cat("Last observed profit =", tail(history$profit, 1), "\n")

#visualization
plot(history$t[history$action == "adjust_price"],
     history$p[history$action == "adjust_price"],
     type = "l", xlab = "t", ylab = "price p",
     main = "Price updates over time")

plot(history$t[history$action == "adjust_taste"],
     history$o[history$action == "adjust_taste"],
     type = "l", xlab = "t", ylab = "taste/style o",
     main = "Taste updates over time")

plot(history$t, history$profit, type = "l",
     xlab = "t", ylab = "profit",
     main = "Profit over time")



