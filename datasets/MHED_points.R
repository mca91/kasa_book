
library(dplyr)
library(tibble)
library(readr)

S <- diag(c(120, 120))

#S_chol <- chol(S)
S_chol <- diag(2)

set.seed(1234)
B <- mvtnorm::rmvnorm(100, mean = c(0, 0)) %*% S_chol 
B[,1] <- B[,1] #+ 35
B[,2] <- B[,2] #+ 90

B <- B %>% 
  as_tibble() %>% 
  mutate(color = "blue")
colnames(B) <- c("x", "y", "color")

K <- mvtnorm::rmvnorm(100, mean = c(0, 0)) %*% S_chol
K[,1] <- K[,1] #+ 65
K[,2] <- K[,2] #+ 110

K <- K %>% 
  as_tibble() %>% 
  mutate(color = "red")
colnames(K) <- c("x", "y", "color")

d <- bind_rows(
 B, K
)

plot(x = d$x, y = d$y, col = d$color, xlim = c(0,100), ylim = c(0, 200), pch = 19, cex = .2)

write_csv(d, file = "~/git_projects/kausalanalyse_book/datasets/MHED_points.csv")


