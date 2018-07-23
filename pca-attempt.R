#Sasan Najar - sasangnajar@gmail.com
#Disc -- PCA

rm(list=ls())
graphics.off()


myData <- read.table("~/Desktop/dataset_prob3.csv", header=TRUE, as.is=TRUE, sep=',')
data <- as.matrix(myData)
data

mean_centered_data <-function(x) {
  scale(x, scale = FALSE)
}

# mean centering using mean subtraction
centered_mean <- function(x) {
  ones = rep(1, nrow(x))
  x_mean = ones %*% t(colMeans(x))
  x - x_mean
}

# apply it
mean_centered_data(myData)
#test if the function is working correctly
centered_mean(myData)

#using covariance matrix
data_pca <- princomp(myData, cor = FALSE)
data_pca$loadings
data_pca$sdev
data_pca$scores



#correlation 
re <- princomp(myData, cor=F)

re
#============= Solution for problem number 2
setwd("/Users/snajar/Desktop")

# 1. Import data
in_file_name <- "dataset_prob3.csv"

myPCA <- function(y) {
  inputData <- read.csv(file=in_file_name,header = TRUE, as.is = TRUE, sep = ",")
  inputData <- inputData[,1:2]
  mean_centering <- scale(inputData, scale = FALSE)
  compute_cov_matrix <- round(cov(mean_centering),30)
  eigen_decomposition <- eigen(compute_cov_matrix)$vectors
  projection <- cbind(eigen_decomposition[,1:2])
  the_scoring_matrix <- mean_centering%*%projection
  #View(mean_centering)
  return(the_scoring_matrix)
}

#call the PCA function
myPCA(inputData)

in_file_name <- read.table("~/Desktop/dataset_prob3.csv", header=TRUE, as.is=TRUE, sep=',')
in_file_name <- in_file_name [,1:2]
x <- in_file_name[,1]
x
y <- in_file_name[,2]
y


#s <- as.matrix(in_file_name)
s <- cbind(x,y)
s

doTheFunction <- myPCA(in_file_name)

a <- doTheFunction[,1]
b <- doTheFunction[,2]

ab <- cbind(a,b)
# raw data in the original coordinate systems
plot(x, y, asp=1, 
     pch=16, cex=1, col="blue",
     main="raw data and PC axis", 
     xlab="V1", ylab="V2",
     xlim=c(-1,1)) # plot raw data


#=========== Solution to number 3
plot(re$sdev^2, pch=16, cex=1, main= "scree plot", xlab="PC", ylab="eigenvalue",xlim=c(-1,1), ylim=c(-1,1))

plot(re$scores[1:20,1], re$scores[1:20, 2],
     pch=16, cex=1,
     xlim=c(min(re$scores[,1]), max(re$scores[,1])),
     ylim=c(min(re$scores[,2]), max(re$scores[,2])),
     main="Scores Plot",
     xlab="PC1", ylab="PC2")

plot(re$loadings[,1], re$loadings[,2], 
     pch=16, cex=1,
     xlab="PC1", ylab="PC2")

#=========== Solution to number 4

#call the PCA function
myPCA(inputData)

in_file_name <- read.table("~/Desktop/dataset_prob4.csv", header=TRUE, as.is=TRUE, sep=',')
in_file_name <- in_file_name [,1:2]
transpose_in_file_name = t(in_file_name)
transpose_in_file_name
x <- transpose_in_file_name[,1]
x
y <- transpose_in_file_name[,2]
y


#s <- as.matrix(in_file_name)
s <- cbind(x,y)
s
doTheFunction <- myPCA(s)

a <- doTheFunction[,1]
b <- doTheFunction[,2]

ab <- cbind(a,b)
# raw data in the original coordinate systems
plot(x, y, asp=1, 
     pch=16, cex=1, col="blue",
     main="raw data and PC axis", 
     xlab="V1", ylab="V2",
     xlim=c(-1,1)) # plot raw data


plot(re$sdev^2, pch=16, cex=1, main= "scree plot", xlab="PC", ylab="eigenvalue",xlim=c(-1,1), ylim=c(-1,1))

plot(re$scores[1:20,1], re$scores[1:20, 2],
     pch=16, cex=1,
     xlim=c(min(re$scores[,1]), max(re$scores[,1])),
     ylim=c(min(re$scores[,2]), max(re$scores[,2])),
     main="Scores Plot",
     xlab="PC1", ylab="PC2")

plot(re$loadings[,1], re$loadings[,2], 
     pch=16, cex=1,
     xlab="PC1", ylab="PC2")