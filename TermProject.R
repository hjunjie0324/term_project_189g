library(recosystem)
library(regtools)

ratingProbsFit <- function(dataIn,maxRating,preMethod,embedMeans,specialArgs){
  colnames(dataIn) <- c("userID", "itemID", "rating")
  if(preMehtod == "logit"){
    lst <- as.list(NULL)
    
    # Check if need to replace userId and itemID by means
    if(embedMeans){
      user_mean <- tapply(dataIn$rating, dataIn$userID, mean)
      item_mean <- tapply(dataIn$rating, dataIn$itemID, mean)

      for(i in (1:nrow(dataIn))){
        user <-dataIn[i,1]
        item <-dataIn[i,2]
        dataIn[i,1]<-user_mean[user]
        dataIn[i,2]<-item_mean[item]
      }
    }
    
    # build models for each single ratings
    for(i in 1: maxRating){
      fixedRating <- as.integer(dataIn$rating == i)
      fixedRating <- as.factor(fixedRating)
      fixedData <- data.frame(dataIn[1:2],fixedRating)

      memory.limit(size = 160000)

      test<-sample(1:nrow(fixedData),0.05*nrow(fixedData))
      utest<-fixedData[test,]
      utrain<-fixedData[-test,]

      glmout <- glm(fixedRating ~ userID + itemID, data = utrain, family = binomial)
      # store the single-rating-model to a list
      lst[[i]] <- glmout

    }
    # store necessary values in a list for later use
    probsFitOut <- list(predMethod = "logit", maxRating = maxRating,lst = lst,dataIn = dataIn)
    class(probsFitOut)<-"recProbs" # make it to to be a class object

  }else if(preMethod == "NMF"){
    if(embedMeans){
      stop("Error: invalid embedMean for NMF\n")  
    }
   
  }else if(preMehod == "kNN"){
    
  }else if(preMethod == "CART"){
    if(!embedMeans){
      stop("Error: invalid embedMean for CART\n")  
    }
  }
  
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
  
  if(probsFitOut$preMehtod == "logit"){
    preds <- NULL
    for(i in 1: probsFitOut$maxRating){
      pred <- predict.glm(probsFitOut$lst[[i]],newXs,type = "response")
      preds <- cbind(preds,pred)
  }
    colnames(preds)<-(1:probsFitOut$maxRating)
   
  }else if(probsFitOut$preMehtod == "NMF"){
   
  }else if(probsFitOut$preMehtod == "kNN"){
    
  }else if(probsFitOut$preMehtod == "CART"){
    
  }
  
  return(preds)
}
