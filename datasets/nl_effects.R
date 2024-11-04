# Notwendige Pakete
library(tibble)
library(dplyr)

# Schritt 1: Datengenerierung
set.seed(1234)
n <- 20000
p <- 10

# Prädiktoren - weniger Noise-Variablen
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", 1:p)

# Schritt 2: Vereinfachter Propensity Score
# Nur von X1 abhängig und weniger extrem
propensity_score <- plogis(0.5 * X[, 1])
B <- rbinom(n, 1, propensity_score)

# Schritt 3: Einfacherer Treatment Effect
# Linear in X1, quadratisch in X2
tau <- 2 + X[, 1] + 0.5 * X[, 2]^2 + X[,3]

# Schritt 4: Einfacheres Outcome-Modell
# Weniger Interaktionen, klarer strukturiert
Y <- tau * B +                    # Behandlungseffekt
  1 + X[, 1] + X[, 2] + X[, 3] +       # Haupteffekte
  rnorm(n, sd = 0.5)           # Reduziertes Rauschen

# Datensatz erstellen
df <- as_tibble(X) %>% 
  mutate(
    Y = Y,
    B = B,
    tau = tau,
    ps = propensity_score
  ) %>%
  # Weniger extreme PS-Trimming
  #filter(between(ps, .2, .8)) %>%
  sample_n(size = 10000)

# Speichern
saveRDS(object = df, file = "datasets/nl_effects.Rds")