library(regtools)

buildGLM <- function(dataIn,maxRating,embedMeans,specialArgs){
  dataIn <- read.csv(dataIn)
  colnames(dataIn) <- c("userID", "itemID", "rating")
  
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
  
  #dataIn$userID <- factorToDummies(dataIn$userID,'user')
  #dataIn$itemID <- factorToDummies(dataIn$itemID,'item')
  
  dummyR <- as.factor(dataIn$rating)
  dummyR <- factorToDummies(dummyR,'rating')
  
  fixedData <- data.frame(dataIn[1:2],dummyR)
  
  glmout <- glm(rating ~ userID + itemID, data = fixedData, family = binomial)
  #glmout <- glm(rating ~ userID, data = fixedData, family = binomial)
  #glmout <- glm(rating ~ itemID, data = fixedData, family = binomial)
  
  # Is this what we are supposed to return? how to parse preMethod? list?
  probsFitOut <- list(preMethod = preMethod,maxRating = maxRating, glmout = glmout)
  class(probsFitOut) <- "recProbs"
  return(probsFitOut)
  
}

predictGlm<-function(prosFitOut,newXs){
  # TODO: need to build a matrix with maxRating
  pred <- predict.glm(glmout,utest,type = "prob") 
}
