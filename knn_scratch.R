
# Setup -------------------------------------------------------------------

library(class)
library(datasets)
library(tidyverse)

# Sample Data -------------------------------------------------------------

# use the iris dataset to get training, validation and testing sets

# train_data - used to train the model
# validation_data - the actual labels from the data set
# test_data - prediction_data with the labels removed used to test the model

random_sample <- sample(1:nrow(iris), 120)

train_data <- iris[random_sample, ]
validation_data <- iris[-random_sample, ]

test_data <- validation_data
test_data$Species <- NULL

# Functions ---------------------------------------------------------------

# input: two 1-dimensional vectors
# output: euclidean distance between the vectors
# euclidean_distance <- function(vector_a, vector_b) {
#   
#   # make sure vectors are one-dimensional
#   if(nrow(vector_a) != 1 | nrow(vector_b) != 1) {
#     stop('The vectors are not 1-dimensional')
#   }
#   
#   # return the euclidian distance between the two vectors
#   return(sqrt(sum((vector_a - vector_b)^2)))
# }

# input: training data frame and test vector
# output: distance between each training row and test vector
euclidean_distance_vectorized <- function(train_df, test_vector) {
  
  # create test data frame of same size as train df
  test_df <- data.frame(matrix(rep(test_vector, each = nrow(train_df)), nrow = nrow(train_df)))
  colnames(test_df) <- c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')
  
  distances <- sapply(apply((apply(test_df,2,as.numeric) - train_df)^2,1,sum),sqrt)
  return(as.vector(distances))
}

# input: train:   train data frame with only the predictor variables
#        test:    test data frame with only the predictor variables
#        k:       the number of nearest neighbors to consider in the probability calculation
#        class_labels: the classification labels of the train data frame
# output: return predicted classification lables for test data
knn_scratch <- function(train, test, k, class_labels) {
  
  # make sure the two data frames have equal number of predictor variables
  if(ncol(train) != ncol(test)) {
    stop('The train and test data sets do not have equal number of predictor variables')
  }
    
  # make sure the train and class_labels are of equal length
  if(nrow(train) != length(class_labels)) {
    stop('Not all labels for train data set are provided')
  }
    
  # make sure the two data frames do not have any null values
  if(any(is.na(train)) | any(is.na(test))) {
    stop('The datasets have null values')
  }
    
  # number of predictors
  p <- ncol(train)
  
  # number of train observations
  n <- nrow(train)
  
  # make sure k is not greater than n
  if(k > n) {
    print('Number of neighbors cannot be greater than the total number of observations, defaulting to the number of observations')
    k <- n
  }
    
  # make sure k is atleast 1
  if(k < 1) {
    print('Number of neighbors should be atleast 1, defaulting to 1 neighbour')
    k <- 1
  }
  
  # convert classes to character
  class_labels <- as.character(class_labels)
  
  # initialize data frame to store the predicted classification labels of the test data
  predict_labels <- character(nrow(test))
  
  # for each observation in the test data
  for(i in 1:nrow(test)) {

    
    # initialize data frame to store the distance of each test observation from the training observation and the corresponding label
    distance_labels <- data.frame(matrix(NA, nrow = n, ncol = 2))
    colnames(distance_labels) <- c('distance', 'label')
    
    # calculate the distance from each train data and also note down the training label
    distance_labels$distance <- euclidean_distance_vectorized(train, test[i,])
    distance_labels$label <- class_labels
    
    # for (j in 1:n) {
    #   distance_labels[j, ]$distance <- euclidean_distance(train[j, ], test[i, ])
    #   distance_labels[j, ]$label <- class_labels[j]
    # }

    # sort by distance
    distance_labels <- distance_labels[order(distance_labels$distance),]
    
    # kth largest distance
    k_dist <- distance_labels[k, ]$distance
    # select all the k nearest distances
    distance_labels <- distance_labels[which(distance_labels$distance <= k_dist[[1]]), ]
    
    # calculate the probability of each class in the neighbors
    distance_labels <- group_by(distance_labels, label)
    probs <- data.frame(count(distance_labels)) 
    probs$n <- probs$n / nrow(distance_labels)
    
    # TODO : what if probabilities are equal?
    
    # sort by the probability
    probs <- probs[order(probs$n),]
    
    # select the class label with max probability
    predict_labels[i] <- probs[1, ]$label
  
  }
  
  return(predict_labels)

}

# Verification ------------------------------------------------------------

scratch_predictions <- knn_scratch(train_data[1:4], test_data, 5, train_data$Species)

knn_predictions <- knn(train_data[1:4], test_data, k = 5, cl = train_data$Species, use.all = T)

compare <- data.frame(test_data, 'Actual' = validation_data$Species, 'Scratch' = scratch_predictions, 'KNN' = knn_predictions)
