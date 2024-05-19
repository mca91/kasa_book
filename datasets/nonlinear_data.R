
library(tidyverse)

d <- read_csv2("~/Downloads/Nonlinear_data.csv") %>% select(x,y)

install.packages("gplm")
library(gplm)

res <- kreg(d$x, d$y, kernel = "uniform", bandwidth = .3, grid = seq(0, 4, .05))

# ground

f <- \(x) sin(2 * x) + 1

plot(d$x, d$y, pch = 20, xlim = c(0,4), )
lines(res, col = "red")
curve(f, from = 0, to = 4, add = T)
