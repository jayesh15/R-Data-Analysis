# Load necessary libraries
library(cluster)
library(stats)

# Display the first 6 rows of the Iris dataset
head(iris, 6)

# Display structure of the Iris dataset
str(iris)

# Remove the Species column from the dataset
iris_1 <- iris[, -5]
head(iris_1)

# Fit K-Means clustering model to the training dataset
kmeans_re <- kmeans(iris_1, centers = 3, nstart = 20)

# Display K-Means clustering results
kmeans_re
kmeans_re$size
kmeans_re$cluster
kmeans_re$centers

# Display confusion matrix and accuracy
true_species <- iris$Species
cluster_assignments <- kmeans_re$cluster
cm <- table(True = true_species, Predicted = cluster_assignments)
acc <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy:", acc))

# Visualize the data and clusters
plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")], col = kmeans_re$cluster, main = "K-means with 3 clusters")

# Visualize cluster centers
kmeans_re$centers
kmeans_re$centers[, c("Sepal.Length", "Sepal.Width")]

# Visualize clusters using clusplot
y_kmeans <- kmeans_re$cluster
clusplot(iris[c("Sepal.Length", "Sepal.Width")], y_kmeans, lines = 0, shade = FALSE, color = TRUE, 
         labels = 2, plotchar = FALSE, span = TRUE, main = "Cluster iris", xlab = 'Sepal.Length', ylab = 'Sepal.Width')
