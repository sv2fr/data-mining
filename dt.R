
# load IRIS dataset ----
library(datasets)

# select only rows with virginica and versicolor ----
data <-
  iris[(iris$Species == 'versicolor' | iris$Species == 'virginica'),]
data$Species <-
  factor(data$Species) # to remove hidden level setosa carried over from original dataset

# summary of data ----
summary(data)

# get predictor, response and class names ----
cols <- colnames(data)
X.col <- cols[1:4]
Y.col <- cols[5]
classes <- levels(data[, Y.col])
positive_class <- classes[1]
negative_class <- classes[2]

# define cutpoints s ----
# note: the min and max x values are not included in the cutpoints
get.cutpoints <- function(n.cutpoints) {
  # get the min of all X
  x.min <- min(data[X.col])
  # get the max of all X
  x.max <- max(data[X.col])
  # create empty matrix of cutpoints initialized with zero
  s <- rep(0, n.cutpoints)
  # difference between each cutpoint
  diff <- (x.max - x.min) / (n.cutpoints + 1)
  # set the first cut-point to x.min + diff (as there will not be any datapoints before x.min)
  s[1] <- x.min + diff
  # form a vector of cutpoints
  for (i in seq(2, n.cutpoints)) {
    s[i] <- s[i - 1] + diff
  }
  return(s)
}
# define number of cutpoints
s <- get.cutpoints(2 * round(max(data[X.col])))

# define mode function ----
mode.categorical <- function(x) {
  return(classes[which.max(tabulate(match(x, classes)))])
}

# define class prediction for each region
# predict.class.label <- function(dataframe) {
#   return(mode.categorical(dataframe[, Y.col]))
# }

# define positive class probability for each region ----
predict.class.probability <- function(dataframe) {
  return(tabulate(match(dataframe[, Y.col], positive_class)) / nrow(dataframe))
}

# define error rate ----
# here we are using the error rate since Y is a qualitative variable
error.rate <- function(dataframe) {
  proportions <-
    tabulate(match(dataframe[, Y.col], classes)) / nrow(dataframe)
  return(1 - max(proportions))
}

# define argmin ----
argmin <- function(dataframe) {
  # argmin row index
  row <- which.min(apply(dataframe, MARGIN = 1, min))[[1]]
  
  # argmin column index
  column <- which.min(dataframe[row,])[[1]]
  
  return(c(row, column))
}

# split the dataframe into two regions based on condition ----
split <- function(dataframe, j, s, condition) {
  # return left region
  if (condition == 'left') {
    return(dataframe[dataframe[, j] < s,])
  }
  
  # return right region
  return(dataframe[dataframe[, j] >= s,])
  
}

# fit tree ----
# change the stopping criteria to get different trees
stop_criteria <- 8 # uses stopping criteria for each region
fit.decision.tree <- function(dataframe) {
  # initialize empty dataframe to store error rates for all combinations of predictor and cutpoint
  # dataframe is initialized with 1 because the error rate can never be greater than 1
  # this will serve as placeholder for combinations where there is no data in the split
  error.combos <-
    data.frame(matrix(1.0, nrow = length(X.col), ncol = length(s)))
  rownames(error.combos) <- X.col
  colnames(error.combos) <- seq(1, length(s))
  
  for (x.col in X.col) {
    for (cutpoint in seq(1, length(s))) {
      # get the indices for which data points are less than the cut off
      split.indices.left <-
        which(dataframe[, x.col] < s[cutpoint])
      
      # get the indices for which data points are greater than or equal to the cut off
      split.indices.right <-
        which(dataframe[, x.col] >= s[cutpoint])
      
      # if number of data points in a region are less than 4 do not split
      if (length(split.indices.left) < stop_criteria |
          length(split.indices.right) < stop_criteria) {
        next
      }
      
      # divide into regions
      R1 <- dataframe[split.indices.left,]
      R2 <- dataframe[split.indices.right,]
      
      # initialize error rate
      R1.error <- 0
      R2.error <- 0
      
      # get the error rate
      R1.error <- error.rate(R1)
      R2.error <- error.rate(R2)
      
      # total error rate
      T.error <- R1.error + R2.error
      
      # fill in error rate
      error.combos[x.col, cutpoint] <- T.error
    }
  }
  
  # if there was no splitting performed return the dataframe
  if (min(error.combos) == 1.0) {
    return(list(data = dataframe))
  }
  
  # get the args for min error rate
  j <- X.col[argmin(error.combos)[1]]
  s <- s[argmin(error.combos)[2]]
  
  split = c(j, s)
  
  # recursive call
  left.subtree = fit.decision.tree(split(dataframe, j, s, 'left'))
  right.subtree = fit.decision.tree(split(dataframe, j, s, 'right'))
  
  tree <-
    list(split = split,
         left.subtree = left.subtree,
         right.subtree = right.subtree)
  return(tree)
}

# predict label of new observation based on tree ----
predict.tree <- function(tree, x) {
  
  j <- tree$split[1]
  s <- tree$split[2]
  
  # predict using data at the leaf node
  if(!is.null(tree$data)) {
    # label <- predict.class.label(tree$data)
    probability <- predict.class.probability(tree$data)
    return(probability)
  }
  
  # if less than cutoff traverse left
  if(x[j] < s) {
    predict.tree(tree$left.subtree, x)
  }
  
  # else traverse right
  else {
    predict.tree(tree$right.subtree, x)
  }
}

# fit tree
tree <- fit.decision.tree(data)

# shuffle iris dataset ----
set.seed(7)
shuffled_data <- data[sample(nrow(data)), ]

# divide into train and validation ----
set.seed(7)
indices <- sample(seq_len(nrow(shuffled_data)), size = nrow(shuffled_data) * 2/3)
train <- shuffled_data[indices, ]
validation <- shuffled_data[-indices, ]

# fit decision tree on train set ----
train_tree <- fit.decision.tree(train)

# predict on validation ----
validation$label <- positive_class
validation$probability <- 0.0
for(i in seq(1, nrow(validation))) {
  validation[i, ]$probability <- predict.tree(train_tree, validation[i, ])
}

# use class probability to make prediction ----
# the threshold is 0.5
validation[which(validation$probability < 0.5), ]$label <- negative_class

# Miss-classification error rate ----
# note: the following miss-classification error rate uses the above threshold of 0.5
confusion_matrix <- table(validation$Species, validation$label)
(confusion_matrix[1, 2] + confusion_matrix[2, 1]) / nrow(validation)

# ROC curve ----
roc_table <- validation[, c('Species', 'probability')]
colnames(roc_table) <- c('actual', 'probability')
# sort probability in decreasing order
roc_table <- roc_table[order(-roc_table$probability), ]

# true positive
roc_table$true_positive <- ifelse(roc_table$actual == positive_class, 1, 0)

# false positive
roc_table$false_positive <- ifelse(roc_table$actual == negative_class, 1, 0)

# ground truth positive
ground_truth <- summary(roc_table$actual)
gp <- ground_truth[[1]]
gn <- ground_truth[[2]]

# cummulative sum tp and fp
for(i in seq(2, nrow(roc_table))) {
  roc_table$true_positive[i] <- roc_table$true_positive[i-1] + roc_table$true_positive[i]
  roc_table$false_positive[i] <- roc_table$false_positive[i-1] + roc_table$false_positive[i]
  
}

# tpr
roc_table$tpr <- roc_table$true_positive / gp

# fpr 
roc_table$fpr <- roc_table$false_positive / gn

# plot roc curve
jpeg('dt_8l_.jpeg')
plot(roc_table$fpr, roc_table$tpr, type = 'b', main = 'Min. number of observations in each region = 8')
dev.off()
