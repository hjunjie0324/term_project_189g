library(regtools)

buildGLM <- function(dataIn,maxRating,embedMeans,specialArgs){
  dataIn <- read.csv(dataIn)
  colnames(dataIn) <- c("userID", "itemID", "rating")
  lst <- as.list(NULL)
  
  if(embedMeans == T){
    user_mean <- tapply(dataIn$rating, dataIn$userID, mean)
    item_mean <- tapply(dataIn$rating, dataIn$itemID, mean)
    
    for(i in (1:nrow(file))){
      user <-dataIn[i,1]
      item <-dataIn[i,2]
      dataIn[i,1]<-user_mean[user]
      dataIn[i,2]<-item_mean[item]
    }
  }
  
  for(i in 1: maxRating){
    fixedRating <- as.factor(dataIn$rating == i)
    fixedData <- data.frame(dataIn[1:2],fixedRating)
    
    memory.limit(size = 160000)
    
    test<-sample(1:nrow(dataIn),0.05*nrow(dataIn))
    utest<-dataIn[test,]
    utrain<-dataIn[-test,]
    
    glmout <- glm(rating ~ userID + itemID, data = utrain, family = binomial)
    lst[[i]] <- glmout
    
  }
  
  
  probsFitOut <- list(predMethod = "logit", maxRating = maxRating,lst = lst,dataIn = dataIn)
  class(probsFitOut)<-"recProbs"
  return(probsFitOut)
  
}

predictGlm<-function(prosFitOut,newXs){
  for(i in 1: maxRating){
    pred <- predict.glm(probFitOut$lst[[i]],newXs,type = "response")
    predlist <- cbind(predlist,pred)
  }
  return(predlist)
}
