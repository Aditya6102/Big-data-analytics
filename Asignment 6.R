# Load necessary libraries
library(ggplot2)
library(cluster)

# Read the CSV file
athelets <- read.csv(file.choose())

# Display the first few rows and column names of the dataset
head(athelets)
print(names(athelets))

# Remove any rows with missing values
athelets <- na.omit(athelets)

# Select the relevant features for clustering
a_data <- athelets[, c("Weight", "Height", "BMI", "Age")]

# Set seed for reproducibility
set.seed(123)

# Perform k-means clustering with 3 clusters
kmeans_result <- kmeans(a_data, centers = 3, nstart = 20)

# Print the results
print(kmeans_result)

# Add the cluster assignments to the original data set
athelets$Cluster <- as.factor(kmeans_result$cluster)

# Plot the clusters (example using Age and BMI)
ggplot(athelets, aes(x = Age, y = BMI, color = Cluster)) +
  geom_point(size = 3) +
  labs(
    x = "Age",
    y = "BMI") +
  theme_minimal()

# Evaluate the clustering with a silhouette plot
silhouette_score <- silhouette(kmeans_result$cluster, dist(a_data))
plot(silhouette_score, col = 1:3, border = NA, main='Plot of Kmeans clustering')