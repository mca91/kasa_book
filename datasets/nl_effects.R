
# Notwendige Pakete
library(tibble)
library(dplyr)

# Schritt 1: Datengenerierung
n <- 5000
p <- 10

# Prädiktoren
set.seed(1234)
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", 1:p)

# Behandlungsindikator B in Abhängigkeit von X1 und X2 (mit PS)
propensity_score <- 1 / (1 + exp(X[, 1] + X[, 2] + X[, 3]))
B <- rbinom(n, 1, propensity_score) 

# Schritt 3: Wahrer ITE ist eine nicht-lineare Funktion von X1, X2 und X3
tau <- (sin(X[, 1] * X[, 2]) + X[, 3]) * 3

# sim. durchschnittlicher Behandlungseffekt
mean(tau)

# Outcome
Y <- tau * B + X[, 1] + X[, 2] + X[, 3] + 0.5 * rnorm(n, sd = 1)

# sammeln
df <- as_tibble(X) %>% mutate(Y = Y, B = B, tau = tau)

# speichern
saveRDS(object = df, file = "datasets/nl_effects.Rds")
