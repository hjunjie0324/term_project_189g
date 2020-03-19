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
    col_name<-c('rating1','rating2','rating3','rating4','rating5')
    for(i in (1:maxRating)){
      dataIn[,i+3]<-0
      names(dataIn)[i+3]<-col_name[i]
    }
    for(i in (1:nrow(dataIn))){
      dataIn[i,dataIn[i,3]+3]=1
    }
    
    probsFitOut <- list(predMethod = "NMF", maxRating = maxRating,dataIn = dataIn)
    class(probsFitOut)<-"recProbs" # make it to to be a class object
   
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
    col_name<-c('rating1','rating2','rating3','rating4','rating5')
    r=Reco()
    maxRating<=probsFitOut$maxRaing
    train<-probsFitOut$dataIn
    test<-newXs
    result<-data.frame(c(test[,1]),c(test[,2]))
    for(i in (1:maxRating)){
      train_set=data_memory(user_index = train$userID,
                            item_index = train$itemID,
                            rating = train[,i+3],
                            index1 = TRUE)
      opts=r$tune(train_set,opts=list(dim=10,niter=100,nmf=TRUE))
      r$train(train_set,opts=c(opts$min,nthread=1,niter=100,nmf=TRUE))
      
      test_set=data_memory(test$userID,test$itemID,index1 = TRUE)
      pred=r$predict(test_set,out_memory())
      
      result<-cbind(result,pred)
    }
    #scale
    rs<-rowSums(result[,3:ncol(result)])
    for(i in (1:nrow(result))){
      result[i,3:ncol(result)]<-result[i,3:ncol(result)]*(1/rs[i])
    }
    names(result)[3:ncol(result)]<-col_name[1:maxRating]
    preds<-result
   
  }else if(probsFitOut$preMehtod == "kNN"){
    
  }else if(probsFitOut$preMehtod == "CART"){
    
  }
  
  return(preds)
}
