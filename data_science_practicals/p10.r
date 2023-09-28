{r}
euclidean_distance <- function(point1, point2) {
  sqrt(sum((point1 - point2)^2))
}

# Function to predict the class for a single data point
knn_predict <- function(train_data, train_labels, new_point, k) {
  # Calculate distances between the new_point and all training points
  distances <- sapply(1:nrow(train_data), function(i) euclidean_distance(train_data[i, ], new_point))
  
  # Find the indices of the k-nearest neighbors
  nearest_indices <- order(distances)[1:k]
  nearest_labels <- train_labels[nearest_indices]
  
  # Find the most common label among the k-nearest neighbors
  predicted_label <- names(sort(table(nearest_labels), decreasing = TRUE)[1])
  return(predicted_label)
}

train_data <- data.frame(
  X1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  X2 = c(2, 3, 1, 5, 4, 6, 7, 9, 8, 10)
)

train_labels <- c("A", "A", "B", "B", "A", "B", "B", "A", "A", "B")

# Test data (single data point to classify)
new_point <- c(X1 = 3.5, X2 = 4.5)

# Number of neighbors to consider (k)
k <- 3

# Predict the class for the test data point
predicted_class <- knn_predict(train_data, train_labels, new_point, k)

# Print the predicted class
cat("Predicted Class:", predicted_class, "\n")