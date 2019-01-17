
# load data
data <- iris
# p : number of features
p <- ncol(data) - 1

# given two vectors x and y return the euclidean distance between them
get_euclid_distance <- function(x, y) {
  return(sqrt(sum((x - y) ^ 2)))
}

# get centroid of dataframe
get_centroid <- function(x) {
  
  center <- vector(mode = 'numeric', length = p)
  for (i in 1:p) {
    center[i] <- mean(x[, i])
  }
  
  return(center)
}

# given two data frames, calculate the sum of distances between them
get_distance_between_centroids <- function(x, y) {
  return(sum(sqrt(rowSums((x - y)^2))))
}

# get within cluster variation - calculated from formula 10.12 R.H.S
get_cluster_variation <- function(data, K) {
  
  cluster_variance <- vector(mode = 'numeric', length = K)
  
  for (i in 1:K) {
    points_in_cluster <- data[data$K == i,]
    centroid <- get_centroid(points_in_cluster)
    cluster_variance[i] <- sum(rowSums((sweep(points_in_cluster[,1:p], 2, centroid, "-"))^2))
  }
  
  return(2 * sum(cluster_variance))
}

# clustering algorithm using k-means
cluster.kmeans <- function(data, K) {
  
  # randomly assign a number between 1 and K to each observation
  set.seed(1) # adding only for reproducability in final output; comment out while evaluation
  data$K <- sample(1:K, nrow(data), replace = TRUE)
  
  # initialize centroid of different clusters
  centroid_old <- as.data.frame(matrix(0, ncol = p, nrow = K))
  centroid_new <- as.data.frame(matrix(0, ncol = p, nrow = K))
  
  # get centroid of different clusters
  for (i in 1:K) {
    points_in_cluster <- data[data$K == i,]
    centroid_old[i,] <- get_centroid(points_in_cluster)
  }
  
  # for each observation assign new cluster based on distance from centroid untill no new assignment(i.e., distance between old and new centroids is zero)
  while (centroid_distance != 0) {
    # for each observation
    for (i in 1:nrow(data)) {
      # calculate distance from each centroid
      distance <- vector(mode = 'numeric', length = K)
      for (j in 1:K) {
        distance[j] <- get_euclid_distance(data[i, 1:p], centroid_old[j,])
      }
      
      # select the cluster with minimum distance from centroid
      data[i,]$K <- which.min(distance)
    }
    
    # calculate new centriod
    for (i in 1:K) {
      points_in_cluster <- data[data$K == i,]
      centroid_new[i,] <- get_centroid(points_in_cluster)
    }
    
    centroid_distance <- get_distance_between_centroids(centroid_old, centroid_new)
    centroid_old <- centroid_new
  }
  
  return(data)
}

# cluster and calculate variation for different K
range_K = 10
variation <- vector(mode = 'numeric', length = K)
for(K in 1:range_K) {
  data <- cluster.kmeans(data, K)
  variation[K] <- get_cluster_variation(data, K)
}

# show variation for each cluster size  
result <- data.frame('K' = c(1:K), variation)
result[order(result$variation), ]

"
For this particular setting, K = 9 gives the least within cluster variation.
    K variation
9   9  1292.879
8   8  1306.180
10 10  1313.950
6   6  1318.738
7   7  1318.809
5   5  1324.993
3   3  1334.081
4   4  1341.585
2   2  1352.352
1   1  1362.741
"
