ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
  colnames(dataIn) <- c('userID','itemID','rating')
  dataIn$userID <- as.factor(dataIn$userID)
  dataIn$itemID <- as.factor(dataIn$itemID)
  if (predMethod=='kNN') {
    
  }
  if (embedMeans){
    userMean <- tapply(dataIn$rating,dataIn$userID,mean)
    itemMean <- tapply(dataIn$rating,dataIn$itemID,mean)
    emb <- dataIn
    emb$userID <- userMean[dataIn$userID]
    emb$itemID <- itemMean[dataIn$itemID]
    emb$userID <- as.vector(emb$userID)
    emb$itemID <- as.vector(emb$itemID)
    result <- emb
  }
  class(result) <- 'recProbs'
  return(result)
}

predict.recProbs <- function(probsFitOut,predMethod,newXs){
  
  if (predMethod=='kNN') {
    library(knnflex)
    x <- rbind(probsFitOut[,1:2],newXs[,1:2])
    kdist <- knn.dist(x)
    cltrn <- probsFitOut[,3]
    #cltst <- newXs$rating
    nrtrn <- nrow(probsFitOut)
    nrtst <- nrow(newXs)
    pred <- knn.predict(1:nrtrn, (nrtrn+1):(nrtrn+nrtst), cltrn, kdist, k=3)
    knnout <- knn.probability(1:nrtrn, (nrtrn+1):(nrtrn+nrtst), cltrn, kdist, k=3)
    knnout <- data.frame(t(knnout))
    colnames(knnout) <- c(1,2,3,4,5)
    return(knnout)
  }
}



library(lme4)
data(InstEval)
dataIn <- InstEval[,c(1,2,7)]
probFitOut <- ratingProbsFit(dataIn = dataIn, maxRating = 5,predMethod = 'kNN',embedMeans = TRUE)
output <- cbind.data.frame(probFitOut$userID,probFitOut$itemID,probFitOut$rating)
sample <- sample(1:nrow(output),5000)
new <- sample(1:nrow(output),1000)
train <- output[sample,]
newXs <- output[new,1:2]
prob <- predict.recProbs(probsFitOut = train, predMethod = 'kNN',newXs = newXs)
