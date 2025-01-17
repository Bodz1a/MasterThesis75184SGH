### This is an example of a script that produces an image and a
### table. These elements are exported to be ready for inclusion in
### the LaTeX document.

### Libraries
library(stargazer)
library(dplyr)

### General options. It is assumed that the working directory is the
### main directory of the repo.
figDir <- "./paper/figs/"
tabDir <- "./paper/tabs/"

### Example of a data frame
n <- 10^3
d <- tibble(
  x = rnorm(n),
  y = 1 + 2 * x + rnorm(n),
  class = ifelse(y > 1 + 2 * x, "Up", "Down")
)

### Exporting small part of the data frame as an example of the data
### structure.
d1 <- d[1:5, ] %>%
  mutate("Values x" = round(x, 2), "Values y" = round(y, 2), "Class" = class) %>%
  select("Values x", "Values y", "Class")
stargazer(d1,
          float = FALSE,
          summary = FALSE,
          out = paste0(tabDir, "tab_00.tex"))

### Simple visualization exported for inclusion in the LaTeX document.
with(d, {
  plot(x, y,
       main = "Example dataset",
       pch = ifelse(class == "Up", 20, 4),
       col = ifelse(class == "Up", rgb(1, 0, 0, .4), rgb(0, 0, 1, .5)))
})
dev.copy(device = png, "./paper/figs/fig_00.png")
dev.off()

### Creating a simple linear regression model.
model <- lm(y ~ +1 +x, data = d)

### Exporting a table related to the model
stargazer(model,
          float = FALSE,
          summary = TRUE,
          out = paste0(tabDir, "tab_01.tex"))

### Creating another visualization 
with(d, {
  plot(x, y,
       main = "Example dataset",
       pch = ifelse(class == "Up", 20, 4),
       col = ifelse(class == "Up", rgb(1, 0, 0, .4), rgb(0, 0, 1, .5)))
  abline(model, lwd = 2, col = "magenta")
})
dev.copy(device = png, "./paper/figs/fig_01.png")
dev.off()

