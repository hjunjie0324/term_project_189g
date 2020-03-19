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
    class(probsFitOut) <- 'recProbs'      
  }else if(preMethod == "CART"){
    if(!embedMeans){
      stop("Error: invalid embedMean for CART\n")  
    }
    if (embedMeans){
      #get the mean value for userid and itemid
      userMean <- tapply(dataIn$rating,dataIn$userID,mean)
      itemMean <- tapply(dataIn$rating,dataIn$itemID,mean)
      emb <- dataIn
      emb$userID <- userMean[dataIn$userID]
      emb$itemID <- itemMean[dataIn$itemID]
      emb$userID <- as.vector(emb$userID)
      emb$itemID <- as.vector(emb$itemID）
    } 
    #cross validation  
    sample <- sample(1:nrow(emb),5000)
    new <- sample(1:nrow(emb),1000)
    train <- emb[sample,]
    newXs <- emb[new,]
    ctout <- ctree(as.factor(rating)~., data=train) #, control=ctree_control(minsplit=2,maxdepth=3,testtype="Teststatistic"))
    dtree <- rpart(as.factor(rating)~.,data=train, method="class",control = rpart.control(cp = 0))  #, maxdepth =20,minsplite=3, minbucket=6
    tree<-prune(dtree,cp=dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"])
    probsFitOut <- list(predMethod = "CART", maxRating = maxRating, lst = tree, newXs = newXs)
    class(probsFitOut) <- 'recProbs'                           
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
    preds <- data.frame(t(knnout))
    colnames(preds) <- c(1,2,3,4,5)
  }else if(probsFitOut$preMehtod == "CART"){
    #prune
    #printcp(tree)
    #plotcp(tree)
    #rpart.plot(dtree,branch=1,type=5, fallen.leaves=T,cex=0.8, sub="no prune")
    #rpart.plot(tree,branch=1, type=2,fallen.leaves=T,cex=0.8, sub="prune")
    test <- probFitOut$newXs
    tree <- probFitOut$lst
    t_pred2 <- predict(tree,test)
    t_expect <- t_pred2%*%c(1,2,3,4,5)
    t_mape <- mean(abs(t_expect-test[,3]))
    t_predtree<-predict(tree,test,type="class")
    t_acc <- count(t_predtree==test[,3])[2,2]/nrow(test)
    #confusionMatrix(table(t_predtree,test$ratings))
    #max_acc <- max(c(ct_acc2,dt_acc,t_acc))
    #min_mape <- min(c(ct_mape,dt_mape,t_mape))
    preds <- t_pred2
  }
  
  return(preds)
}
