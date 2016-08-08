# Reset R session
rm(list=ls())
cat("\014")

myFunc <- function(nReps, myScatterInput, myClusterNum){
  ## Define variables for the function
  # seed
  s = 1001
  
  # number of rows and columns
  n <- nrow(myScatterInput)
  m <- ncol(myScatterInput)
  
  # convert the data frame into a matrix
  scatterMatrix <- as.matrix(x = myScatterInput)
  
  bestRep_euclideanSum <- Inf
  
  ## Repeat nReps times
  for(r in 1:nReps){
    set.seed(s)
    
    # randomly choose <myClusterNum> cluster points
    clusterCenters <- as.matrix(scatterMatrix[sample(1:n, myClusterNum),])
    newClusterCenters <- clusterCenters
    
    # pre-allocate matrix with euclidean distances
    distances <- matrix(NA, nrow = n, ncol = myClusterNum)
    
    ## Repeat up to <maxIter> times, or newClusterCenters == clusterCenters
    iter <- 0
    while(iter < maxIter){
      
      # get Euclidean distance from each point to all clusters
      for(i in 1:nrow(clusterCenters)){
        distances[, i] <- apply(scatterMatrix, 1, function(x) sqrt(sum((x-clusterCenters[i,])^2)))
      }
      
      # assign each point to the cluster center closest to it
      belongToCluster <- apply(distances, 1, function(x) which(x == min(x, na.rm = T))[1])
      
      # update clusters
      for(i in 1:myClusterNum){
        newClusterCenters[i,] <- apply(as.matrix(scatterMatrix[belongToCluster == i,]), 2, mean)
      }
      
      # break condition (newClusterCenters == clusterCenters)
      if(isTRUE(all.equal(newClusterCenters, clusterCenters))) break
      # else update clusterCenters
      clusterCenters <- newClusterCenters
      
      iter <- iter + 1
    }
    
    ## Compute sum of all Euclidean distances from each point to their cluster center
    euclideanSum <- 0
    for(i in 1:n){
      euclideanSum <- euclideanSum + distances[i, belongToCluster[i]]
    }
    
    ## Store information about this repetition if it's the best
    if(euclideanSum < bestRep_euclideanSum){
      bestRep <- r
      bestRep_belongToCluster <- belongToCluster
      bestRep_euclideanSum <- euclideanSum
    }
    
    s <- s + 1
  }
  
  ## Print the best result
  print(paste("The repetition for which we get the lowest sum of Euclidean distances is number ", bestRep,
              ", for which we get a value of ", round(bestRep_euclideanSum, 3), ".", sep=""))
  
  ## If m = 2, make a 2d scatter plot
  if(m == 2){
    plot(scatterMatrix, type = "p", col = bestRep_belongToCluster,
         main = "Clustering result")
  }
  
  ## If m == 3, make a 3d scatter plot
  if(m == 3){
    if(!"scatterplot3d" %in% rownames(installed.packages())){
      install.packages("scatterplot3d", dependencies = T)
    }
    library(scatterplot3d)
    
    scatterplot3d(scatterMatrix, color = bestRep_belongToCluster,
                  main = "Clustering result")
  }
}

## Read input data
myData <- read.csv("hw3data.csv", header = F)
myParams <- read.csv("hw3params.csv", header = F)

## Get parameters
nReps <- myParams[[1]]
myClusterNum <- myParams[[2]]
maxIter <- myParams[[3]]

## Run function
myFunc(nReps, myData, myClusterNum)
