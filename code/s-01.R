### Cleaning
rm(list = ls())

### Libraries
### library(dplyr)


# discrete time t = 1, ..., T (T_max);
# N customers with ideal prices and reservation price;
# 2 firms with fixed quality of goods which are deciding on prices;
# the algorithm of buying something is stochasti;c
# the pricing of companies is based on profits and prices for the two previous periods (as we discussed at the meeting)



# DEMAND FUNCTION

# Returns the vector (demand1, demand2) for given prices and qualities
simulate_demand <- function(agents, price1, price2, q1, q2,
                            alpha_dist = 3, gamma_price = 2) {

  r  <- agents$reservation
  id <- agents$ideal_quality
  util0 <- 0 # not to buy
  # if price > reservation -> usefulness -Inf, agent does not buy
  util1 <- ifelse(price1 > r,
    -Inf,
    -alpha_dist * abs(id - q1) - gamma_price * (price1 / r))
  
  util2 <- ifelse(price2 > r,
    -Inf,
    -alpha_dist * abs(id - q2) - gamma_price * (price2 / r))
  
  # Softmax for obtained probabilities
  max_u <- pmax(util0, util1, util2) # always 0
  e0 <- exp(util0 - max_u) # always 1
  e1 <- exp(util1 - max_u)
  e2 <- exp(util2 - max_u)
  
  denom <- e0 + e1 + e2
  p0 <- e0 / denom     # probability not to buy
  p1 <- e1 / denom     # to buy from firm 1
  p2 <- e2 / denom     # to buy from firm 2
  
  # stochastic choice of every agent
  u <- runif(length(r))
  choice <- ifelse(u < p1,
    1,
    ifelse(u < p1 + p2, 2, 0))
  
  demand1 <- sum(choice == 1)
  demand2 <- sum(choice == 2)
  
  return(c(demand1 = demand1, demand2 = demand2))
}

# Rule for updating company prices based on profit/prices
# profit up, price up     -> price up
# profit up, price down   -> price down
# profit down, price up   -> price down
# profit down, price down -> price up

update_price <- function(prev_price, prev_prev_price,
                         prev_profit, prev_prev_profit,
                         step, p_min = 0.1) {
  d_p <- prev_price - prev_prev_price
  d_pi <- prev_profit - prev_prev_profit
  
  direction <- 0
  
  if (d_pi > 0 && d_p > 0) {
    # 1
    direction <- +1
  }
  if (d_pi > 0 && d_p < 0) {
    # 2
    direction <- -1
  }
  if (d_pi < 0 && d_p > 0) {
    # 3
    direction <- -1
  }
  if (d_pi < 0 && d_p < 0) {
    # 4
    direction <- +1
  }

  ## equalities
  if (d_pi == 0 | d_p == 0) {
    # direction <- sample(x = c(-1, 1), size = 1, prob = c(1, 1)/2) # choose direction at random
    direction <- 0 # do not make a change
  }
  
  new_price <- prev_price + direction * step
  new_price <- max(new_price, p_min)
  return(new_price)
}

# set.seed(123)


# PARAMETERS

T_max <- 10000  # time (number of periods)
N_agents <- 1000 # agents (customers)
q1 <- 0.3 # quality of goods
q2 <- 0.7
c1 <- 2 # marginal costs of firms
c2 <- 2
p1 <- double(T_max) #initial prices as for t1 and t2
p2 <- double(T_max)

p1[1] <- 10.0
p2[1] <- 15.0


p1[2] <- p1[1] * 1.05  # firm 1 makes prices a bit higher
p2[2] <- p2[1] * 0.95  # and 2 a bit lower

price_step <- 0.1 # step of price changing
price_floor <- 0.1 # minimal price allowed
alpha_dist  <- 3   # sensibility to distance between ideal and possibility
gamma_price <- 2   # sensibility to price / reservation

# GENERATION OF AGENTS POPULATION

# reservation price
# reservation_raw <- rnorm(n = N_agents, mean = 10, sd = 2)
reservation_raw <- runif(n = N_agents, min = 0.1, max = 20)
reservation <- ifelse(reservation_raw > 0.5, reservation_raw, 0.5)

# ideal quality
ideal_quality <- runif(N_agents, min = 0, max = 1)

agents <- data.frame(
  reservation   = reservation,
  ideal_quality = ideal_quality
)

# Main simulation cycle by time

demand1 <- numeric(T_max)
demand2 <- numeric(T_max)
profit1 <- numeric(T_max)
profit2 <- numeric(T_max)

# calculating periods 1 and 2 with prices set initially
for (t in 1:2) {
  d <- simulate_demand(agents, p1[t], p2[t], q1, q2,
                       alpha_dist = alpha_dist, gamma_price = gamma_price)
  demand1[t] <- d["demand1"]
  demand2[t] <- d["demand2"]
  profit1[t] <- (p1[t] - c1) * demand1[t]
  profit2[t] <- (p2[t] - c2) * demand2[t]
}

# then t = 3, ..., T_max. 
# first we update prices according to the rule, then we calculate demand and profit
for (t in 3:T_max) {
  # frim1
  p1[t] <- update_price(
    prev_price      = p1[t - 1],
    prev_prev_price = p1[t - 2],
    prev_profit     = profit1[t - 1],
    prev_prev_profit= profit1[t - 2],
    step            = price_step,
    p_min           = price_floor
  )
  
  # frim2
  p2[t] <- update_price(
    prev_price      = p2[t - 1],
    prev_prev_price = p2[t - 2],
    prev_profit     = profit2[t - 1],
    prev_prev_profit= profit2[t - 2],
    step            = price_step,
    p_min           = price_floor
  )
  
  # demand and revenue in new prices
  d <- simulate_demand(agents, p1[t], p2[t], q1, q2,
                       alpha_dist = alpha_dist, gamma_price = gamma_price)
  demand1[t] <- d["demand1"]
  demand2[t] <- d["demand2"]
  
  profit1[t] <- (p1[t] - c1) * demand1[t]
  profit2[t] <- (p2[t] - c2) * demand2[t]
}

# results

results <- data.frame(
  t       = 1:T_max,
  price1  = p1,
  price2  = p2,
  demand1 = demand1,
  demand2 = demand2,
  profit1 = profit1,
  profit2 = profit2
)

`%L%` <- paste0
file_name <- "data" %L% as.character(Sys.time()) %L% ".csv"
write.csv(results, file_name, row.names = FALSE)
  
# results

## # charts
## pdf(file = "test.pdf")
## par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

## # prices
## matplot(results$t, cbind(results$price1, results$price2),
##         type = "l", lty = 1, xlab = "t", ylab = "Price",
##         main = "Prices of firms 1 and 2")
## legend("topright", legend = c("Firm 1", "Firm 2"), lty = 1,
##        col = 1:2, bty = "n")

## # profit
## matplot(results$t, cbind(results$profit1, results$profit2),
##         type = "l", lty = 1, xlab = "t", ylab = "Profit",
##         main = "Profits of firms 1 and 2")
## legend("topright", legend = c("Firm 1", "Firm 2"), lty = 1,
##        col = 1:2, bty = "n")

## # demand
## matplot(results$t, cbind(results$demand1, results$demand2),
##         type = "l", lty = 1, xlab = "t", ylab = "Demand",
##         main = "Demand for firms 1 and 2")
## legend("topright", legend = c("Firm 1", "Firm 2"), lty = 1,
##   col = 1:2, bty = "n")

## dev.off()
