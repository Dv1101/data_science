# Load required libraries
library(readr)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)  # Required for scaling

# Load the dataset
df <- read.csv("D:/programs/data_science/Crop_recommendation.csv")

# Display dataset summary statistics
df_description <- summary(df)

# Create a boxplot
ggplot(df, aes(x = ph, y = label)) +
  geom_boxplot() +
  labs(x = "pH", y = "Label") +
  theme_minimal()

# Convert the 'label' column to categorical
df$label <- as.factor(df$label)

# Split the data into features (X) and target (y)
y <- df$label
X <- df[, c("N", "P", "K", "temperature", "humidity", "ph", "rainfall")]

# Split the data into training and test sets
set.seed(1)  # For reproducibility
splitIndex <- createDataPartition(y, p = 0.75, list = FALSE)
X_train <- X[splitIndex, ]
y_train <- y[splitIndex]
X_test <- X[-splitIndex, ]
y_test <- y[-splitIndex]

# Scale the features using Min-Max scaling
scaler <- preProcess(X_train, method = c("range"))
X_train_scaled <- predict(scaler, X_train)
X_test_scaled <- predict(scaler, X_test)

# K-Nearest Neighbors Classifier
knn <- train(X_train_scaled, y_train, method = "knn")
knn_score <- knn$results$Accuracy[1]

# K-Nearest Neighbors Classifier with different k values
k_range <- 1:10
knn_scores <- numeric(length(k_range))
for (k in k_range) {
  knn_model <- train(X_train_scaled, y_train, method = "knn", tuneGrid = data.frame(k = k))
  knn_scores[k] <- knn_model$results$Accuracy
}

# Decision Tree Classifier
decision_tree <- train(X_train, y_train, method = "rpart")
decision_tree_score <- decision_tree$results$Accuracy[1]

# Random Forest Classifier
random_forest <- randomForest(X_train, y_train, ntree = 100, mtry = 4)
rf_train_accuracy <- sum(random_forest$confusion[1, ]) / sum(random_forest$confusion)
rf_test_accuracy <- sum(predict(random_forest, newdata = X_test) == y_test) / length(y_test)

# Gradient Boosting Classifier
gradient_boosting <- train(X_train, y_train, method = "gbm")
gb_accuracy <- gradient_boosting$results$Accuracy[1]

# Print results
cat("Dataset Summary Statistics:\n")
print(df_description)
cat("\nK-Nearest Neighbors Classifier Accuracy:", knn_score)
cat("\nDecision Tree Classifier Accuracy:", decision_tree_score)
cat("\nRandom Forest Classifier - Training Accuracy:", rf_train_accuracy)
cat("\nRandom Forest Classifier - Test Accuracy:", rf_test_accuracy)
cat("\nGradient Boosting Classifier Accuracy:", gb_accuracy)

# Plot K-Nearest Neighbors Classifier Accuracy vs. k values
plot(k_range, knn_scores, type = "b", xlab = "k", ylab = "Accuracy", xlim = c(1, 10), ylim = c(0.96, 0.99))
abline(h = max(knn_scores), col = "red", lty = 2)