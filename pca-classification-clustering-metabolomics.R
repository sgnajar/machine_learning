# Sasan Najar - sasangnajar@gmail.com
# A pipeline to carry our dimension reduction, clustering, and classification analysis of a metabolomics dataset.
# 1. dimention reduction
# 2. clustering
# 3. classification

rm(list=ls())
graphics.off()


setwd("D:\\Documents\\GitHub\\machine_learning") 
myData <- "ST000003_peaks_table.csv"
  
data_in <- read.csv(file=myData, header=TRUE)
                
dataForAnalysisA <- data_in[1:144, 8:26]
dataForAnalysisA
# first: do the PCA
re <- princomp(dataForAnalysisA, cor=FALSE)


#screeplot
plot(re$sdev^2,
     pch=16, cex=1, col="blue",
     main="Scree plot",
     xlab="PCA index", ylab="eigenvalues")

# scores plot
plot(re$scores[,1], re$scores[,2],
     pch=16, cex=1, col="blue",
     xlab="PC1", ylab="PC2",
     main="PCA scores plot")

# loading plot
plot(re$loadings[, 1], re$loadings[, 2],
     asp=1,
     pch=16, cex=1, col="blue",
     xlab="PC1", ylab="PC2",
     main="Loadings plot")

# second: kmean clustering
re.kmeans <- kmeans(x=dataForAnalysisA, centers=dataForAnalysisA[1:2,], algorithm="Lloyd")
re.kmeans$size

re.PCA <- prcomp(x=dataForAnalysisA, scale=F) #change to T
plot(re.PCA$sdev^2,
     pch=16, cex=1,
     xlab="PC number", ylab="variance",
     main="scree plot after standardization of variables")

# third: classification 
packageName <- "MASS"
install.packages(packageName)
library(packageName, character.only = T)

head(dataForAnalysisA[,1:19])

re.LDA.MASS <- lda(x=dataForAnalysisA[,1:18], grouping=dataForAnalysisA$number_of_observation_per_peak,tol=1.0e-4, method="mle", CV=F)

#Error in lda.default(x, grouping, ...) : 
#group means are numerically identical

nrow(dataForAnalysisA)
head(dataForAnalysisA)
dataForAnalysisA

#since the data is only using group number 18 it's giving error of "group means are numerically identical"
#however I tested other version (DiscriMiner) and its working well

# apply LDA function in DiscriMiner
packageName <- "DiscriMiner"
install.packages(packageName)
library(packageName, character.only = T)
length(grouping)

re.LDA.DiscriMiner <- linDA(variables=dataForAnalysisA[,1:18], group=dataForAnalysisA$number_of_observation_per_peak,validation="crossval")

re.LDA.DiscriMiner$functions
re.LDA.DiscriMiner$confusion
re.LDA.DiscriMiner$scores
re.LDA.DiscriMiner$classification
re.LDA.DiscriMiner$error_rate

#####################################################

dataForAnalysisBtemp <- data_in[,8:26]

dataForAnalysisB <- dataForAnalysisBtemp[!is.na(dataForAnalysisBtemp)]


sum(is.na(dataForAnalysisBtemp))

# first: do the PCA
re <- princomp(dataForAnalysisB, cor=FALSE)

re

#screeplot
plot(re$sdev^2,
     pch=16, cex=1, col="blue",
     main="Scree plot",
     xlab="PCA index", ylab="eigenvalues")

# scores plot
plot(re$scores[,1], re$scores[,2],
     pch=16, cex=1, col="blue",
     xlab="PC1", ylab="PC2",
     main="PCA scores plot")

# loading plot
plot(re$loadings[, 1], re$loadings[, 2],
     asp=1,
     pch=16, cex=1, col="blue",
     xlab="PC1", ylab="PC2",
     main="Loadings plot")

# second: kmean clustering
re.kmeans <- kmeans(x=dataForAnalysisB, centers=dataForAnalysisB[1:2,], algorithm="Lloyd")
re.kmeans$size

re.PCA <- prcomp(x=dataForAnalysisB, scale=F) #change to T
plot(re.PCA$sdev^2,
     pch=16, cex=1,
     xlab="PC number", ylab="variance",
     main="scree plot after standardization of variables")


# third: classification 
packageName <- "MASS"
install.packages(packageName)
library(packageName, character.only = T)

head(dataForAnalysisB[,1:19])

re.LDA.MASS <- lda(x=dataForAnalysisB[,1:18], grouping=dataForAnalysisB$number_of_observation_per_peak,tol=1.0e-4, method="mle", CV=F)



