#' @title Causal Random Forest
#' @description This function implements a Causal Random Forest algorithm to estimate treatment effects from observational data. It allows for the estimation of heterogeneous treatment effects by leveraging the random forest framework.
#' @name causal_random_forest
#' @param X A data frame or matrix of covariates (features).
#' @param Y A numeric vector of outcomes (response variable).
#' @param W A binary vector indicating treatment assignment (1 for treated, 0 for control).
#' @param num.trees The number of trees to grow in the random forest.
#' @param mtry The number of features to consider at each split.
#' @param min.node.size The minimum size of terminal nodes.
#' @param sample.fraction The fraction of samples to use for each tree.
#' @param alpha The significance level for the confidence intervals.
#' @return A list containing estimated treatment effects and other relevant statistics.
#' @import randomForest
#' @import dplyr
#' @import purrr
#' @export
causal_random_forest <- function(X, Y, W, num.trees = 500, mtry = NULL, min.node.size = 5, sample.fraction = 0.632, alpha = 0.05) {
  # Check inputs
  if (length(Y) != length(W) || nrow(X) != length(W)) {
    stop("Length of Y and W must match the number of rows in X.")
  }
  
  # Initialize variables
  n <- nrow(X)
  if (is.null(mtry)) {
    mtry <- floor(sqrt(ncol(X)))
  }
  
  # Fit the random forest model for treated and control groups
  rf_treated <- randomForest(x = X[W == 1, ], y = Y[W == 1], ntree = num.trees, mtry = mtry, nodesize = min.node.size, sampsize = floor(sample.fraction * sum(W == 1)))
  rf_control <- randomForest(x = X[W == 0, ], y = Y[W == 0], ntree = num.trees, mtry = mtry, nodesize = min.node.size, sampsize = floor(sample.fraction * sum(W == 0)))
  
  # Predict treatment effects
  pred_treated <- predict(rf_treated, newdata = X)
  pred_control <- predict(rf_control, newdata = X)
  treatment_effects <- pred_treated - pred_control
  
  # Calculate confidence intervals
  se <- sqrt(var(treatment_effects) / length(treatment_effects))
  ci_lower <- treatment_effects - qnorm(1 - alpha / 2) * se
  ci_upper <- treatment_effects + qnorm(1 - alpha / 2) * se
  
  # Return results
  return(list(treatment_effects = treatment_effects, ci_lower = ci_lower, ci_upper = ci_upper))
}