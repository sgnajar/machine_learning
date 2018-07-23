#Sasan Najar - sasangnajar@gmail.com
#Disc -- Kmeans clustering


#kmeans(x, centers, iter.max = 10, nstart = 1,
#       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", MacQueen"), trace=FALSE)


#K means function that takes data with 2 or 3 variables, a fixed cluster number, 
#and iterates over the k means cluster algorithm a maximum of maxIter times, or until the 
#cluster assignments do not change. This functions repeats nReps times and finds the lowest
#distance for the cluster assignments over all nReps.



###############################################################################
myKmeans <- function(x,kCluster,nIteration) {
  clusterRecord <- vector(nIteration, mode = "list")
  centerRecord <- vector(nIteration, mode = "list")

  #nIteration = 3
  for (i in 1:nIteration)
    {
    
    dataMatrix <- as.matrix(x)
    #Randomly choose kCluster points from the input. 
    #As the starting cluster centroids.
    centroids <- dataMatrix[sample(nrow(dataMatrix), kCluster), ]
    #set old and new cluster 
    oldClusters<-rep(0,nrow(dataMatrix))
    newClusters<-rep(1,nrow(dataMatrix))
    
    # Make new centroids by taking the mean of all the points in each cluster 
    # repeat until the cluster assignments do not change or until the max iteration
    count <- 0 
    while(count < nIteration & identical(oldClusters,newClusters) == FALSE) {
      count <- count + 1
      oldClusters <- newClusters
      #check the distances of each data point to each of the centroids
      dist_to_center <- euclid(dataMatrix, centroids)
      #find the centroid that is closest to each datapoint - and assign datapoint to that cluster
      newClusters <- apply(dist_to_center, 1, which.min)
      #recalculate new centroids but computing the average of all datapoints in each cluster
      centroids <- sapply(1:ncol(dataMatrix),
                        function(k) sapply(1:kCluster, 
                        function(y) mean(dataMatrix[ which(newClusters == y),k],na.rm=TRUE)))
                        
      #end of while loop
    }
    
    #Once you have reached a terminating condition, 
    #compute the sum of all Euclidean distances from each point to their respective centroids.
    #for row x, x want to take the euclidean distance of row x with 
    sumDists <- sapply(1:nrow(dataMatrix), 
                       function(k) euclid2(dataMatrix[k,], 
                                           centroids[newClusters[k],]))
    tempSum <- sum(sumDists)
    
    # Identify the replication with the lowest sum of Euclidean distances from points to centroids 
    #as your best result and print the value to the console.
    if (i==1) {
      finalSum <<- tempSum
      finalClusters <<- newClusters
    }else{
      if(tempSum < finalSum){
        finalSum <<- tempSum
        finalClusters<<-newClusters
        minRep <<- i
      } 
    }
    #end of xReps
  }
  data_matrix<-cbind(dataMatrix,finalClusters)
  mydata <- list(x,kCluster,nIteration,centroids)
  mydata$x
  return(mydata)
  cat(sprintf("Input matrix %s", x, "number of cluster %s", kCluster ,
           "number of iteration %s", nIteration))
  
  
  #end of kMeans
}

#euclidean distance matrix for multiple points

euclid <- function(points1, points2) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}

#euclidean distance value for two points
euclid2 <- function(row1, row2) {
  distance<-0
  for(i in 1:length(row2)) {
    distance<-distance+(row1[i]-row2[i])^2
  }
  distance<-sqrt(distance)
}


#run the code  #####################################################
# prepare matrix of data 
cells <- c(1, 1, 2, 1, 4, 3, 5, 4)
rnames <- c("A", "B", "C", "D")
cnames <- c("X", "Y")
x <- matrix(cells, nrow=4, ncol=2, byrow=TRUE, dimnames=list(rnames, cnames)) 
#x <-matrix(c(1, 1, 2, 1, 4, 3, 5, 4), ncol=2, byrow=TRUE)

centers <- x[sample(nrow(x) , 3)]  #sample some centers, 3 for example
kCluster = 2
#nIteration = 3


results <- myKmeans(x, kCluster=2, nIteration=3)
results

cat(sprintf("number of iterations %s", nIteration))
cat(sprintf("the size of clusters %s", kCluster))

results$kCluster
nrow(x)
size(x)
x$size

#re.kmeans <- kmeans(x=data_for_analysis, centers=data_for_analysis[1:2,], algorithm="Lloyd", iter.max=100)


#to test if the function is working properly or not:
ff <- kmeans (x, centers=x[1:2,], algorithm="Lloyd", iter.max=3)
ff



## modelled on print methods in the cluster package
print.kmeans <- function(x, ...)
{
  cat("K-means clustering with ", length(x$size), " clusters of sizes ",
      paste(x$size, collapse=", "), "\n", sep="")
  cat("\nCluster means:\n")
  print(x$centers, ...)
  cat("\nClustering vector:\n")
  print(x$nIteration, ...)
  cat("\nWithin cluster sum of squares by cluster:\n")
  print(x$withinss, ...)
  cat(sprintf(" (between_SS / total_SS = %5.1f %%)\n",
              100 * x$betweenss/x$totss),
      "Available components:\n", sep="\n")
  print(names(x))
  invisible(x)
}

fitted.kmeans <- function(object, method = c("centers", "classes"), ...)
{
  method <- match.arg(method)
  if (method == "centers") object$centers[object$cl, , drop=FALSE]
  else object$cl
}



########## hierarchical clustering

# the input matrix x. The output[i,j] position is the Euclidean distance
# between rows x[i,] and x[j,].
dis = function(x)
{
  x = as.matrix(x)
  u = apply(x*x,1,sum) %*% matrix(1.0,1,nrow(x))
  sqrt(abs(u + t(u) - 2 * x %*% t(x)))
}

# The following ordering function for the hclust plot is translated from the
# original Fortran code used by R in hclust.f. It's only needed by the plotting
# routine to avoid crossing connections. This translation is really really slow.
iorder = function(m)
{
  N = nrow(m) + 1
  iorder = rep(0,N)
  iorder[1] = m[N-1,1]
  iorder[2] = m[N-1,2]
  loc = 2
  for(i in seq(N-2,1))
  {
    for(j in seq(1,loc))
    {
      if(iorder[j] == i)
      {
        iorder[j] = m[i,1]
        if(j==loc)
        {
          loc = loc + 1
          iorder[loc] = m[i,2]
        } else
        {
          loc = loc + 1
          for(k in seq(loc, j+2)) iorder[k] = iorder[k-1]
          iorder[j+1] = m[i,2]
        }
      }
    }
  }
  -iorder
}

hc = function(d, method=c("single","complete","average","median"))
  {
  if(!is.matrix(d)) d = as.matrix(d)
  # Pick a clustering function:
  method_fn = switch(match.arg(method),
                     single   = min,
                     complete = max,
                     average  = mean,
                     median  = median)
  N = nrow(d)
  diag(d)=Inf
  n = -(1:N)                       # Tracks group membership
  m = matrix(0,nrow=N-1, ncol=2)   # hclust merge output
  h = rep(0,N-1)                   # hclust height output
  for(j in seq(1,N-1))
  {
    # Find smallest distance and corresponding indices
    h[j] = min(d)
    # We can use == here (I think) because we know we'll get exactly a 0 value.
    i = which(d - h[j] == 0, arr.ind=TRUE)
    # We could get more than one, but we just want to merge one pair, so take 1st.
    i = i[1,,drop=FALSE]
    p = n[i]
    # R's convention is to order each m[j,] pair as follows:
    p = p[order(p)]
    m[j,] = p
    # Agglomerate this pair and all previous groups they belong to
    # into the current jth group:
    grp = c(i, which(n %in% n[i[1,n[i]>0]]))
    n[grp] = j
    # Concoct replacement distances that consolidate our pair using `method`:
    r = apply(d[i,],2,method_fn)
    # Move on to the next minimum distance, excluding current one by modifying
    # the distance matrix:
    d[min(i),] = d[,min(i)] = r
    d[min(i),min(i)]        = Inf
    d[max(i),] = d[,max(i)] = Inf
  }
  # Return something similar to the output from hclust.
  
  structure(list(merge = m, height = h, order = iorder(m),
                 labels = rownames(d), method = method, 
                 call = match.call(), dist.method = "euclidean"), 
            class = "hclust")
}

################## 
distMatrix <- dist(x=x, method="euclidean")
# complete linkage
re.hclust <- hclust(d=distMatrix, method="complete")
cutoff <- 2.5e+6

# vertical dendrogram
plot(re.hclust, main="dist = euclidean, linkage = complete", sub="",
     xlab="", ylab="dissimilarity", 
     hang=-1)
points(c(0, nrow(x)), c(cutoff, cutoff), 
       type="l", lwd=2, lty=2,
       col="red")

# horizontal dendrogram
plot(as.dendrogram(re.hclust), horiz=T, 
     main="dist = euclidean, linkage = complete", 
     xlab="dissimilarity", ylab="")
points(c(cutoff, cutoff), c(0, nrow(data_for_analysis)), 
       type="l", lwd=2, lty=2,
       col="red")



re.hclust$order

# first merge
re.hclust$merge[1,]
re.hclust$labels[c(27,28)]
merge_dist <- re.hclust$height[1]
points(c(merge_dist, merge_dist), c(0, nrow(x)),
       type="l", lwd=2, lty=2,
       col="green")

merge_dist
sqrt(sum((x[27,]-x[28,])^2))


# second merge
re.hclust$merge[2,]
re.hclust$labels[c(25,26)]
merge_dist <- re.hclust$height[2]
points(c(merge_dist, merge_dist), c(0, nrow(x)),
       type="l", lwd=2, lty=2,
       col="green")

merge_dist
sqrt(sum((x[25,]-x[26,])^2))


