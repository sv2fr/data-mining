
# Setup -------------------------------------------------------------------

install.packages('plot3D')
install.packages('ployly')

library(plot3D)
library(plotly)

# Functions ---------------------------------------------------------------

# input: any scalar 
# output: value on the standard normal curve
standard.normal.univariate <-  function(x) {
  
  return((1 / sqrt(2 * pi)) * exp(-(x^2) / 2))
  
}

# input: two dimensional point (x1, x2) 
# output: value on the standard normal curve
standard.normal.bivariate <- function(x) {
  
  x1 <- x[1]
  x2 <- x[2]
  z <- x1^2 + x2^2
  return((1 / sqrt(2 * pi)) * exp(-(z)/ 2))

}

# KDE Functions ---------------------------------------------------------------

# input: x - point at which we want to estimate the value using kde
#        sample - vector or points in the original distribution
#        bandwidth - the flexibility parameter
# output: estimated value at the point using kde
est.kde.univariate <-  function(x, sample, bandwidth) {
  n <- length(sample)
  h <- bandwidth
  k <- standard.normal.univariate((x - sample)/ h)
  return((1/n*h) * sum(k))
}

# input: x - 2 dimensional point at which we want to estimate the value using kde
#        sample - vector of two dimensional points
#        bandwidth - the flexibility parameter
# output: estimated value at the point using kde
est.kde.bivariate <-  function(x, sample, bandwidth) {
  n <- nrow(sample)
  h <- bandwidth
  dist <- -sweep(sample, 2, x)
  k <- apply(dist, 1, standard.normal.bivariate)
  return((1/n*h) * sum(k))
}

# Sample Data -------------------------------------------------------------

# univariate ----
data.univariate <- c(rnorm(300, mean = 3, sd = 1), rnorm(300, mean = 9, sd = 1))
sample.data.univariate <- seq(min(data.univariate), max(data.univariate), length = 300)

# bi-variate ----
x1.data1 <- rnorm(300, mean = 3, sd = 1)
x2.data1 <- rnorm(300, mean = 3, sd = 1)
b.data1 <- cbind(x1.data1, x2.data1)

x1.data2 <- rnorm(300, mean = 9, sd = 1)
x2.data2 <- rnorm(300, mean = 9, sd = 1)
b.data2 <- cbind(x1.data2, x2.data2)

data.bivariate <- as.matrix(rbind(b.data1, b.data2))
colnames(data.bivariate) <- c("x1","x2")

sample.data.bivariate <- data.bivariate[sample(nrow(data.bivariate), size = 300), ]

# Effect of Bandwidth -----------------------------------------------------

# univariate ----

# draw histogram
hist(data.univariate, breaks = 100, freq = F)
rug(data.univariate, ticksize=0.03, side=1, lwd=0.1)

# draw lines for different bandwidth values from estimates
est.univariate <- sapply(sample.data.univariate, est.kde.univariate, sample = data.univariate, bandwidth = 1.5)
lines(sample.data.univariate, est.univariate, col = 'red', lwd = 1.5, type = 'o', cex = 1.5)

est.univariate <- sapply(sample.data.univariate, est.kde.univariate, sample = data.univariate, bandwidth = 1.3)
lines(sample.data.univariate, est.univariate, col = 'blue', lwd = 1.5, type = 'o', cex = 1.5)

est.univariate <- sapply(sample.data.univariate, est.kde.univariate, sample = data.univariate, bandwidth = 1)
lines(sample.data.univariate, est.univariate, col = 'yellow', lwd = 1.5, type = 'o', cex = 1.5)

est.univariate <- sapply(sample.data.univariate, est.kde.univariate, sample = data.univariate, bandwidth = 0.8)
lines(sample.data.univariate, est.univariate, col = 'green', lwd = 1.5, type = 'o', cex = 1.5)

est.univariate <- sapply(sample.data.univariate, est.kde.univariate, sample = data.univariate, bandwidth = 0.5)
lines(sample.data.univariate, est.univariate, col = 'violet', lwd = 1.5, type = 'o', cex = 1.5)

est.univariate <- sapply(sample.data.univariate, est.kde.univariate, sample = data.univariate, bandwidth = 0.3)
lines(sample.data.univariate, est.univariate, col = 'orange', lwd = 1.5, type = 'o', cex = 1.5)

est.univariate <- sapply(sample.data.univariate, est.kde.univariate, sample = data.univariate, bandwidth = 0.1)
lines(sample.data.univariate, est.univariate, col = 'pink', lwd = 1.5, type = 'o', cex = 1.5)

# bi-variate ----

# draw 3d histogram of data
x1cut <- cut(data.bivariate[,1], 60)
x2cut <- cut(data.bivariate[,2], 60)

cuts <- table(x1cut, x2cut)
hist3D(z=cuts, freq = F, border = 'black', image = T)

# draw 3d histogram of data we want to estimate
x1cut <- cut(sample.data.bivariate[,1], 60)
x2cut <- cut(sample.data.bivariate[,2], 60)

cuts <- table(x1cut, x2cut)
hist3D(z=cuts, freq = F, border = 'black', image = T)

# 2d visualization of the points
scatter2D(x = data.bivariate[,1], y = data.bivariate[,2])

# interactive graph of estimates for different bandwidths
est.bivariate <- apply(sample.data.bivariate, 1, est.kde.bivariate, sample = data.bivariate, bandwidth = 1.7)
z1 <- as.matrix(cbind(sample.data.bivariate, est.bivariate))
p1 <- plot_ly(x = z1[,1], y = z1[,2], z = z1[,3], type = 'mesh3d')

est.bivariate <- apply(sample.data.bivariate, 1, est.kde.bivariate, sample = data.bivariate, bandwidth = 1.3)
z2 <- as.matrix(cbind(sample.data.bivariate, est.bivariate))
p2 <- plot_ly(x = z2[,1], y = z2[,2], z = z2[,3], type = 'mesh3d')

est.bivariate <- apply(sample.data.bivariate, 1, est.kde.bivariate, sample = data.bivariate, bandwidth = 1)
z3 <- as.matrix(cbind(sample.data.bivariate, est.bivariate))
p3 <- plot_ly(x = z3[,1], y = z3[,2], z = z3[,3], type = 'mesh3d')

est.bivariate <- apply(sample.data.bivariate, 1, est.kde.bivariate, sample = data.bivariate, bandwidth = 0.8)
z4 <- as.matrix(cbind(sample.data.bivariate, est.bivariate))
p4 <- plot_ly(x = z4[,1], y = z4[,2], z = z4[,3], type = 'mesh3d')

est.bivariate <- apply(sample.data.bivariate, 1, est.kde.bivariate, sample = data.bivariate, bandwidth = 0.5)
z5 <- as.matrix(cbind(sample.data.bivariate, est.bivariate))
p5 <- plot_ly(x = z5[,1], y = z5[,2], z = z5[,3], type = 'mesh3d')

est.bivariate <- apply(sample.data.bivariate, 1, est.kde.bivariate, sample = data.bivariate, bandwidth = 0.3)
z6 <- as.matrix(cbind(sample.data.bivariate, est.bivariate))
p6 <- plot_ly(x = z6[,1], y = z6[,2], z = z6[,3], type = 'mesh3d')

# NOTE: grpah can be rotated interactively to view the shape
p <- subplot(p1, p2, p3, p4, p5, p6)
p
