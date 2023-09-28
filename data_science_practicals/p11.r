library(ggplot2)

set.seed(123)
data <- data.frame(
  X1 = rnorm(100, mean = 0, sd = 1),
  X2 = rnorm(100, mean = 0, sd = 1)
)

k <- 3
kmeans_result <- kmeans(data, centers = k)

cluster_assignments <- kmeans_result$cluster
cluster_centers <- kmeans_result$centers

print(cluster_assignments)
print(cluster_centers)

clustered_data <- cbind(data, Cluster = factor(cluster_assignments))

ggplot(clustered_data, aes(x = X1, y = X2, color = Cluster)) +
  geom_point() +
  geom_point(data = as.data.frame(cluster_centers), aes(x = X1, y = X2), color = "black", size = 3, shape = 4) +
  labs(title = "K-Means Clustering") +
  theme_minimal()
