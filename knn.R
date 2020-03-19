
ratingProbsFit <- function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
  colnames(dataIn) <- c('userID','itemID','rating')
  dataIn$userID <- as.factor(dataIn$userID)
  dataIn$itemID <- as.factor(dataIn$itemID)
  
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
  
  if (predMethod=='kNN') {
    library(knnflex)
    #5-fold cross validation to choose k
    num <- 5000
    xsample <- sample(1:nrow(result),num)
    xtest <- result[xsample,]
    xtest<-xtest[sample(nrow(xtest)),]
    folds<-cut(seq(1,nrow(xtest)),breaks = 5,labels = FALSE)
    err <- rep(0,5)
    acc <- rep(0,5)
    
    for(i in 1:5){
      testIndexes <- which(folds==i,arr.ind = TRUE)
      testData <- xtest[testIndexes, 1:2]
      trainData <- xtest[-testIndexes, 1:2]
      data <- rbind(trainData,testData)
      kdist <- knn.dist(data)
      cl <- dataIn$rating[xsample]
      cltrn <- cl[-testIndexes]
      cltst <- cl[testIndexes]
      pred <- knn.predict(1:(num*0.8),(num*0.8+1):num, cltrn, kdist, k=i+2)
      knnout <- knn.probability(1:(num*0.8),(num*0.8+1):num, cltrn, kdist, k=i+2)
      knnout <- data.frame(t(knnout))
      colnames(knnout) <- c(1,2,3,4,5)
      err[i] <- mean(abs(pred-cltst))
      
    }
    k1 <- which.min(err)
    
    
    probsFitOut <- list(predMethod='kNN',dataIn=result,k=k1+2)
  }
  
  class(probsFitOut) <- 'recProbs'
  return(probsFitOut)
}

predict.recProbs <- function(probsFitOut,newXs){
  
  if (probsFitOut$predMethod=='kNN') {
    library(knnflex)
    
    userMean <- tapply(newXs$rating,newXs$userID,mean)
    itemMean <- tapply(newXs$rating,newXs$itemID,mean)
    emb <- newXs
    emb$userID <- userMean[newXs$userID]
    emb$itemID <- itemMean[newXs$itemID]
    emb$userID <- as.vector(emb$userID)
    emb$itemID <- as.vector(emb$itemID)
    
    output <- probsFitOut$dataIn[,1:2]
    sample <- sample(1:nrow(output),5000) 
    train <- output[sample,]
    #5000 ratings where chosen as train data
    x <- rbind(train,emb[,1:2])
    kdist <- knn.dist(x)
    cltrn <- probsFitOut$dataIn$rating[sample]
    cltst <- newXs$rating
    nrtrn <- nrow(train)
    nrtst <- nrow(newXs)
    pred <- knn.predict(1:nrtrn, (nrtrn+1):(nrtrn+nrtst), cltrn, kdist, k=probFitOut$k)
    knnout <- knn.probability(1:nrtrn, (nrtrn+1):(nrtrn+nrtst), cltrn, kdist, k=probFitOut$k)
    knnout <- data.frame(t(knnout))
    colnames(knnout) <- c(1,2,3,4,5)
    
    MAPE <- mean(abs(pred-cltst))
    preds <- round(pred)
    acc <- sum(preds==cltst)/nrow(newXs)
    
    return(knnout)
  }
}



library(lme4)
data(InstEval)
dataIn <- InstEval[,c(1,2,7)]


#setwd('D:/UCD/ECS189G/HW/Final')
#song <- read.table('songsDataset.csv',sep=',',header = TRUE)
#head(song)
#dataIn <- song

probFitOut <- ratingProbsFit(dataIn = dataIn, maxRating = 5,predMethod = 'kNN',embedMeans = TRUE)

new <- sample(1:nrow(dataIn),1000)
newXs <- dataIn[new,]
colnames(newXs) <- c('userID','itemID','rating')

prob <- predict.recProbs(probsFitOut = probFitOut, newXs = newXs)







