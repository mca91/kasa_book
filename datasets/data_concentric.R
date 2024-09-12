# Setze den Zufallszahlengenerator
set.seed(123)

# Erstellen der Daten: Zwei konzentrische Kreise
n <- 200  # Anzahl der Datenpunkte pro Klasse
r1 <- sqrt(runif(n))  # Radius für die erste Klasse
theta1 <- runif(n, 0, 2*pi)  # Winkel für die erste Klasse
x1 <- cbind(r1 * cos(theta1), r1 * sin(theta1))

r2 <- sqrt(runif(n)) + 1  # Radius für die zweite Klasse
theta2 <- runif(n, 0, 2*pi)  # Winkel für die zweite Klasse
x2 <- cbind(r2 * cos(theta2), r2 * sin(theta2))

# Daten zusammenfügen
X <- rbind(x1 = x1, x2 = x2)
y <- factor(c(rep(1, n), rep(2, n)))

data_concentric <- bind_cols(
  X1 = X[,1],
  X2 = X[,2],
  klasse = y
)

saveRDS(object = data_concentric, file = "datasets/data_concentric.RDS")
