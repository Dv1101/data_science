{r}
# Linear Regression Class
LinearRegression <- function(learning_rate = 0.01, num_iterations = 1000) { 
  model <- list()
  model$learning_rate <- learning_rate
  model$num_iterations <- num_iterations
  model$weights <- NULL
  model$bias <- NULL
  
  fit <- function(X, y) {
    num_samples <- nrow(X)
    num_features <- ncol(X)
    
    # Initialize weights and bias
    model$weights <- matrix(0, ncol = num_features, nrow = 1) # Initialize as a row matrix
    model$bias <- 0
    
    # Gradient descent for optimization
    for (i in 1:model$num_iterations) {
      # Predictions with current weights and bias
      y_pred <- X %*% t(model$weights) + model$bias
      
      # Compute gradients
      dw <- (1 / num_samples) * t(X) %*% (y_pred - y)
      db <- (1 / num_samples) * sum(y_pred - y)
      
      # Update weights and bias
      model$weights <- model$weights - model$learning_rate * dw
      model$bias <- model$bias - model$learning_rate * db
    }
  }
  
  predict <- function(X) {
    return (X %*% t(model$weights) + model$bias)
  }
  
  return(list(fit = fit, predict = predict))
}

x <- c(151, 120, 138, 186, 122, 136, 179, 163, 152, 131)
y <- c(63, 78, 56, 91, 47, 98, 76, 72, 62, 48)

data <- data.frame(x = x, y = y)
relation <- lm(y ~ x, data = data)
new_data <- data.frame(x = 170)
result <- predict(relation, newdata = new_data)
print(result)