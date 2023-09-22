# Load required libraries
library(randomForest)
library(ggplot2)
library(caret)

# Load the dataset
dataset <- read.csv("D:/programs/data_science/Social_Network_Ads.csv")

# Split the dataset into features (X) and target variable (y)
X <- dataset[, c("Age", "EstimatedSalary")]
y <- dataset[, "Purchased"]

# Split the dataset into training and test sets
set.seed(0)  # For reproducibility
splitIndex <- createDataPartition(y, p = 0.75, list = FALSE)
X_train <- X[splitIndex, ]
y_train <- y[splitIndex]
X_test <- X[-splitIndex, ]
y_test <- y[-splitIndex]

# Standardize the features
X_train <- scale(X_train)
X_test <- scale(X_test)

# Create a random forest classifier
classifier <- randomForest(x = X_train, y = y_train, ntree = 10, mtry = 2, nodesize = 1)

# Predict on the test set
y_pred <- predict(classifier, newdata = X_test)

# Confusion Matrix
cm <- table(Actual = y_test, Predicted = y_pred)

# Plot the decision boundary for the test set
grid <- expand.grid(Age = seq(min(X_test[, 1]) - 1, max(X_test[, 1]) + 1, by = 0.01),
                    EstimatedSalary = seq(min(X_test[, 2]) - 1, max(X_test[, 2]) + 1, by = 0.01))
grid$y_pred <- predict(classifier, newdata = grid)

ggplot() +
  geom_contour(data = grid, aes(x = Age, y = EstimatedSalary, z = y_pred),
               bins = 2, alpha = 0.75, fill = c("red", "green")) +
  geom_point(data = dataset, aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased))) +
  scale_color_manual(values = c("red", "green")) +
  xlim(min(X_test[, 1]) - 1, max(X_test[, 1]) + 1) +
  ylim(min(X_test[, 2]) - 1, max(X_test[, 2]) + 1) +
  labs(title = "Random Forest Classification (Test set)",
       x = "Age", y = "Estimated Salary") +
  theme_minimal()
