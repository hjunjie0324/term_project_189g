library(regtools)

buildGLM <- function(dataIn,maxRating,embedMeans){
  lst <- as.list(NULL)
  
  if(embedMeans){
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
    fixedRating <- as.integer(dataIn$rating == i)
    fixedRating <- as.factor(fixedRating)
    fixedData <- data.frame(dataIn[1:2],fixedRating)
    
    memory.limit(size = 160000)
    
    test<-sample(1:nrow(fixedData),0.05*nrow(fixedData))
    utest<-fixedData[test,]
    utrain<-fixedData[-test,]
    
    glmout <- glm(fixedRating ~ userID + itemID, data = utrain, family = binomial)
    lst[[i]] <- glmout
    
  }
  
  
  probsFitOut <- list(predMethod = "logit", maxRating = maxRating,lst = lst,dataIn = dataIn)
  class(probsFitOut)<-"recProbs"
  return(probsFitOut)
  
}

predictGlm<-function(probsFitOut,newXs){
  predlist <- NULL
  for(i in 1: probsFitOut$maxRating){
    pred <- predict.glm(probsFitOut$lst[[i]],newXs,type = "response")
    predlist <- cbind(predlist,pred)
  }
  colnames(predlist)<-(1:probsFitOut$maxRating)
  return(predlist)
}

ratingProbsFit <- function(dataIn,maxRating,preMethod,embedMeans,specialArgs){
  
  probsFitOut <- switch(preMethod, "logit" = buildGLM(dataIn,maxRating,embedMeans))
  return(probsFitOut)
}

predict.recProbs <- function(probsFitOut,newXs){
  # source: https://www.rdocumentation.org/packages/prob/versions/1.0-1/topics/setdiff
  isNewUser = setdiff(newXs$userID,probsFitOut$dataIn$userID)
  isNewItem = setdiff(newXs$itemID,probsFitOut$dataIn$itemID)
  
  if(!identical(isNewUser,integer(0)) || !identical(isNewItem,integer(0))){
    # source:https://stat.ethz.ch/R-manual/R-devel/library/base/html/stop.html
    stop("ERROR: NEW USER OR NEW ITEM\n") 
  }
  
  
  preds<-switch(probsFitOut$predMethod, "logit" = predictGlm(probsFitOut,newXs))
                #"knn" = (),"NMF" = (),"CART" = ())
  
  return(preds)
}


library(regtools)

setwd('C:/omsi-master')

dataIn<- read.csv('songsDataset.csv')
colnames(dataIn) <- c("userID", "itemID", "rating")

ppp <- ratingProbsFit(dataIn,5,"logit",F,specialArgs)
newXs <- data.frame(userID = 0,itemID = 7171)

newXs$userID <- as.integer(newXs$userID)
newXs$itemID <- as.integer(newXs$itemID)

isNewUser = setdiff(newXs$userID,dataIn$userID)
#isNewUser
#typeof(isNewUser)


a<-predict.recProbs(ppp,newXs)
a
  

