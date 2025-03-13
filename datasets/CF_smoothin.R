# Load necessary libraries
library(ggplot2)
library(reshape2)
library(grf)

# Define grid for x1 and x2 (covariates)
x1 <- seq(0, 1, length.out = 100)
x2 <- seq(0, 1, length.out = 100)

# Complicated function to define treatment effect
true_treatment_effect <- function(x1, x2) {
  term1 <- 5 * (1 - exp(-(2 * x1^2 + 2 * x2^2))) + 2  
  term2 <- sin(3 * pi * x1) + cos(3 * pi * x2)        
  term4 <- exp(-x1 * x2)                       
  return(term1 + term2 + term4)
}
# Simulate treatment assignment and random points
set.seed(123)
n_random <- 10000
x1_random <- runif(n_random)
x2_random <- runif(n_random)
treatment <- rbinom(n_random, 1, 0.5)

# Create true treatment effect and outcomes
TE_random <- true_treatment_effect(x1_random, x2_random)
outcome_random <- function(x1, x2, treatment, TE) {
  return(2 + x1 + x2 + treatment * TE + rnorm(length(x1), 0, 1))
}

# Data frame for causal forest analysis
df_random <- data.frame(
  x1 = x1_random,
  x2 = x2_random,
  treatment = treatment,
  outcome = outcome_random(x1_random, x2_random, treatment, TE_random)
)

# Fit causal forest with tuning
X_random <- as.matrix(df_random[, c("x1", "x2")])
Y_random <- df_random$outcome
W_random <- df_random$treatment
causal_forest_tuned <- causal_forest(
  X_random, Y_random, W_random,
  tune.parameters = "all")

# Fit causal forest without tuning
causal_forest_untuned <- causal_forest(X_random, Y_random, W_random)

# Generate a grid of x1 and x2 for prediction
grid_data <- expand.grid(x1 = x1, x2 = x2)
X_grid <- as.matrix(grid_data)  # Convert grid data to matrix format

# Predictions with both tuned and untuned models
grid_data$pred_CATE_tuned <- predict(causal_forest_tuned, X_grid)$predictions
grid_data$pred_CATE_untuned <- predict(causal_forest_untuned, X_grid)$predictions

# Fit a linear regression model with interaction terms
linear_model <- lm(outcome ~ (x1 + x2 + I(x1^2) + I(x2^2) + I(x1^3) + I(x2^3)) * treatment, 
                   data = df_random)
  
# Predict treatment effects using the linear model
grid_data$pred_CATE_lm <- predict(linear_model, newdata = data.frame(
  x1 = grid_data$x1,
  x2 = grid_data$x2,
  treatment = 1
)) - predict(linear_model, newdata = data.frame(
  x1 = grid_data$x1,
  x2 = grid_data$x2,
  treatment = 0
))

# Plot function to avoid code repetition
plot_CATE <- function(data, CATE_col, title, legendpos = "none") {
  ggplot(data, aes(x1, x2, fill = !!sym(CATE_col))) +
    geom_tile() +
    scale_fill_viridis_c(option = "mako") +
    labs(title = title, x = "Variable x1", y = "Variable x2", fill = "Predicted CATE") +
    theme_minimal() +
    theme(legend.position = legendpos, 
          axis.title.y = element_blank(), 
          axis.title.x = element_blank()
    )
}

# Plot the true treatment effects
p_truth <- ggplot(melt(outer(x1, x2, true_treatment_effect)), aes(Var1 / 100, Var2 / 100, fill = value)) +
  geom_hex(stat = "identity") +
  scale_fill_viridis_c(option = "mako") +
  labs(title = "True Treatment Effects", fill = "TE") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank()
  )

# Plot predicted CATEs with and without tuning, and from the linear model
p_CF_untuned <- plot_CATE(grid_data, "pred_CATE_untuned", "Est. CATE without Tuning")
p_CF_tuned <- plot_CATE(grid_data, "pred_CATE_tuned", "Est. CATE with Tuning")
p_lm <- plot_CATE(grid_data, "pred_CATE_lm", "Est. CATE from Linear Model")

library(cowplot)

plot_grid(
 plotlist = list(
   p_truth, p_lm, p_CF_untuned, p_CF_tuned
),
  ncol = 2
)

