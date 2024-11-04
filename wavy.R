# Load necessary libraries
library(ggplot2)
library(e1071)

# Function to generate uniform grid data with a wavy decision boundary
generate_grid_wavy_data <- function(n, noise_level = 0.2) {
  # Generate uniformly distributed points across a grid
  x1 <- runif(n, -5, 5)
  x2 <- runif(n, -5, 5)
  
  # Define the wavy boundary: y = x + sin(x * 2)
  decision_boundary <- x1 + sin(x1 * 1.2) * 3
  
  # Assign classes based on whether the points are above or below the boundary, with noise
  class <- ifelse(x2 > decision_boundary + rnorm(n, sd = noise_level), 1, 2)
  
  # Create the data frame
  data <- tibble(x1 = x1, x2 = x2, klasse = factor(class))
  
  return(data)
}

# Set seed for reproducibility
set.seed(1234)
# Generate the wavy grid data
n <- 2000  # Number of points
noise_level <- 1  # Noise level to add some randomness to the decision boundary

data_wavy <- generate_grid_wavy_data(n, noise_level)
saveRDS(object = data_wavy, file = "datasets/data_wavy.RDS")


# Visualize the data
ggplot(data = svm_wavy, aes(x = x1, y = x2, color = klasse)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Uniform Grid Data with Wavy Decision Boundary and Noise")

# Train a support vector machine on the data using a radial basis kernel
svm_model <- svm(
  klasse ~ x2 + x1, 
  data = svm_wavy, 
  kernel = "radial", 
  cost = 1, 
  gamma = 6
)

# Plot the SVM decision boundary
plot(svm_model, svm_wavy)











library(e1071)
library(rgl)
library(dplyr)

# Trainiere das SVM-Modell mit dem Radial Basis Function (RBF)-Kernel
radial_svm_model <- svm(
  klasse ~ x2 + x1, 
  data = svm_wavy, 
  kernel = "radial", 
  cost = 1,
  gamma = 1
)

# Erstelle einen Grid für Vorhersagen
grid <- expand_grid(
  x1 = seq(min(svm_wavy$x1), max(svm_wavy$x1), length.out = 150),
  x2 = seq(min(svm_wavy$x2), max(svm_wavy$x2), length.out = 150)
) 

# Berechne die Vorhersagen und erzeuge eine Z-Achse, die den transformierten Raum repräsentiert
grid <- grid %>%
  mutate(
    klasse = predict(radial_svm_model, grid),
    z = exp(-1 * (x1^2 + x2^2))  # Radial-Basis-Funktion (RBF) als Beispiel für Transformation
  )

# Originaldaten in den 3D-Raum transformieren
svm_wavy <- svm_wavy %>%
  mutate(
    z = exp(-1 * (x1^2 + x2^2))  # Gleiche Transformation wie für den Grid
  )

# 3D-Plot mit rgl, um die Punkte im transformierten Raum zu visualisieren
plot3d(svm_wavy$x1, svm_wavy$x2, svm_wavy$z, col = as.numeric(svm_wavy$klasse), size = 5, type = "s", xlab = "X1", ylab = "X2", zlab = "Z (Kernel Transform)")

# Grid der transformierten Punkte hinzufügen
points3d(grid$x1, grid$x2, grid$z, col = as.numeric(grid$klasse), size = 0.5)

# Titel hinzufügen
title3d("Transformierter Raum durch RBF-Kernel", xlab = "X1", ylab = "X2", zlab = "Z (Kernel Transform)")
