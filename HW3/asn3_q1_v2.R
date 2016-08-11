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
  
  # define the return elements
  #returnClusterCenters <- list()
  #returnClusterCenters <- matrix(NA, nrow = nReps, ncol = myClusterNum)
  #returnBelongToCluster <- matrix(NA, nrow = n, ncol = nReps)
  #returnEuclideanSums <- matrix(NA, nrow = 1, ncol = nReps)
  
  euclideanSum <- Inf
  
  ## Repeat nReps times
  for(r in 1:nReps){
    set.seed(s)
    
    # randomly choose <myClusterNum> cluster points
    clusterCenters <- scatterMatrix[sample(1:n, myClusterNum),]
    newClusterCenters <- clusterCenters
    
    # pre-allocate matrix with euclidean distances
    #distances <- matrix(NA, nrow = n, ncol = myClusterNum)
    
    # pre-allocate matrix with belongings (which cluster center is closest to each point)
    #belongToCluster <- matrix(NA, nrow = n, ncol = 1)
    
    ## Repeat up to <maxIter> times, or newClusterCenters == clusterCenters
    iter <- 0
    while(iter < maxIter){
      
      belongToCluster <- apply(mat, 1,
                               function(x) which(
                                 c(sqrt(sum((x-clusterCenters[1,])^2)),
                                   sqrt(sum((x-clusterCenters[2,])^2)),
                                   sqrt(sum((x-clusterCenters[3,])^2))) == min(
                                     c(sqrt(sum((x-clusterCenters[1,])^2)),
                                       sqrt(sum((x-clusterCenters[2,])^2)),
                                       sqrt(sum((x-clusterCenters[3,])^2))))))
      
      # # get Euclidean distance from each point to all clusters
      # for(i in 1:nrow(clusterCenters)){
      #   #distances[, i] <- apply(scatterMatrix, 1, function(x) dist(rbind(x, clusterCenters[i,])))
      #   distances[,i] <- apply(scatterMatrix, 1, function(x) sum(sqrt((x-clusterCenters[i,])^2)))
      # }
      # 
      # # assign each point to the cluster center closest to it
      # for(i in 1:n){
      #   belongToCluster[i, 1] <- which(distances[i, 1:myClusterNum] == min(distances[i, 1:myClusterNum], na.rm = T))[1]
      # }
      
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
    newEuclideanSum <- 0
    for(i in 1:myClusterNum){
      #euclideanSum <- euclideanSum + distances[i, belongToCluster[i,]]
      newEuclideanSum <- newEuclideanSum + sum(apply(as.matrix(scatterMatrix[belongToCluster==i,]), 1,
                                                     function(x) sqrt(sum((x-clusterCenters[i,])^2))))
    }
    
    if(newEuclideanSum < euclideanSum){
      euclideanSum <- newEuclideanSum
      bestRep_belongToCluster <- belongToCluster
      bestRep_index <- r
    }
    
    ## Store information about this repetition
    #returnClusterCenters[[r]] <- clusterCenters
    #returnBelongToCluster[, r] <- belongToCluster
    #returnEuclideanSums[1, r] <- euclideanSum
    
    s <- s + 1
  }
  
  ## Find replication for which we get he lowest Euclidean sum
  #bestRep <- which(returnEuclideanSums == min(returnEuclideanSums))[1]
  #print(paste("The repetition for which we get the lowest sum of Euclidean distances is number ", bestRep,
  #            ", for which we get a value of ", round(min(returnEuclideanSums), 3), ".", sep=""))
  print(paste("The repetition for which we get the lowest sum of Euclidean distances is number ", bestRep_index,
              ", for which we get a value of ", round(euclideanSum), ".", sep=""))
  
  ## Save information belonging to the best repetition
  #bestRep_clusterCenters <- returnClusterCenters[[bestRep]]
  #bestRep_belongToCluster <- returnBelongToCluster[, bestRep]
  #bestRep_euclideanSums <- returnEuclideanSums[1, bestRep]
  
  ## If m = 2, plot the set
  if(m == 2){
    #par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    plot(scatterMatrix, type = "p", col = bestRep_belongToCluster,
         main = "Clustering result", xlab = "x", ylab = "y")
    #legend("topright", inset=c(-0.2,0), legend=1:myClusterNum,
    #       col=1:myClusterNum, title="Clusters", pch=1)
  }
  
  ## If m == 3, make a 3d scatter plot
  if(m == 3){
    if(!"scatterplot3d" %in% rownames(installed.packages())){
      install.packages("scatterplot3d", dependencies = T)
    }
    library(scatterplot3d)
    
    #par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    scatterplot3d(scatterMatrix, color = bestRep_belongToCluster,
                  main = "Clustering result", xlab = "x", ylab = "y", zlab = "z")
    #legend("topright", inset=c(-0.2,0), legend=1:myClusterNum,
    #       col=1:myClusterNum, title="Clusters", pch=1)
  }
  
  #return(list(bestRep_clusterCenters, bestRep_belongToCluster, bestRep_euclideanSums))
}

## Read input data
#myData <- read.csv("hw3data.csv", header = F)
#myParams <- read.csv("hw3params.csv", header = F)

myData <- read.csv("hw3data_2d_ds.csv", header = F)
#myData <- read.csv("hw3data_3d_ds.csv", header = F)
myParams <- read.csv("hw3param_ds.csv", header = F)
nReps <- myParams[[1]]
myClusterNum <- myParams[[2]]
maxIter <- myParams[[3]]

## Run function
myFunc(nReps, myData, myClusterNum)
