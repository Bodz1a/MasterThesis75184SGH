### Cleaning
rm(list = ls())

### Libraries
library(dplyr)

get_price1 <- function(f) {
  d0 <- read.csv(file = f, header = TRUE)
  d0$price1
}

get_price2 <- function(f) {
  d0 <- read.csv(file = f, header = TRUE)
  d0$price2
}


for (j in 1:20) {
  system("Rscript s-01.R")
  print(j)
}

### All files
fs <- list.files(path = "./", pattern = "csv$")

g1 <- lapply(X = fs, FUN = get_price1)
g2 <- lapply(X = fs, FUN = get_price2)

plot(
  apply(X = matrix(unlist(g1), ncol = length(g1)), MARGIN = 1, FUN = mean),
  pch = 20, type = "l", col = "steelblue3", lwd = 4, ylim = c(0, 40))
grid(col = "steelblue1", lty = "solid")

lines(
  apply(X = matrix(unlist(g1), ncol = length(g1)), MARGIN = 1, FUN = function(x) {mean(x) - sd(x)}),
  pch = 20, type = "l", col = "pink2", lwd = 1)

lines(
  apply(X = matrix(unlist(g1), ncol = length(g1)), MARGIN = 1, FUN = function(x) {mean(x) + sd(x)}),
  pch = 20, type = "l", col = "pink2", lwd = 1)


lines(
  apply(X = matrix(unlist(g2), ncol = length(g2)), MARGIN = 1, FUN = mean),
  pch = 20, type = "l", col = "purple2", lwd = 4)

lines(
  apply(X = matrix(unlist(g2), ncol = length(g2)), MARGIN = 1, FUN = function(x) {mean(x) - sd(x)}),
  pch = 20, type = "l", col = "orange3", lwd = 1)

lines(
  apply(X = matrix(unlist(g2), ncol = length(g2)), MARGIN = 1, FUN = function(x) {mean(x) + sd(x)}),
  pch = 20, type = "l", col = "orange3", lwd = 1)



