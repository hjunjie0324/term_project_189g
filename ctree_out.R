ratingProbsFit <-function(dataIn,maxRating,predMethod,embedMeans,specialArgs){
  library(lme4)
  library(regtools)
  library(rectools)
  library(recosystem)
  library(partykit)
  library(rpart)
  library(rpart.plot)
  library(caret)
  library(car)
  library(plyr)
  
  #dataset
  colnames(dataIn) <- c('userID','itemID','rating')
  dataIn$userID <- as.factor(dataIn$userID)
  dataIn$itemID <- as.factor(dataIn$itemID)
  
  if (embedMeans){
    #get the mean value for userid and itemid
    userMean <- tapply(dataIn$rating,dataIn$userID,mean)
    itemMean <- tapply(dataIn$rating,dataIn$itemID,mean)
    emb <- dataIn
    emb$userID <- userMean[dataIn$userID]
    emb$itemID <- itemMean[dataIn$itemID]
    emb$userID <- as.vector(emb$userID)
    emb$itemID <- as.vector(emb$itemID)
    
  }
  
  #cross validation  
  sample <- sample(1:nrow(emb),5000)
  new <- sample(1:nrow(emb),1000)
  train <- emb[sample,]
  newXs <- emb[new,]
  
  
  
  
  if (predMethod=="CART"){
    
    ctout <- ctree(as.factor(rating)~., data=train) #, control=ctree_control(minsplit=2,maxdepth=3,testtype="Teststatistic"))
    dtree <- rpart(as.factor(rating)~.,data=train, method="class",control = rpart.control(cp = 0))  #, maxdepth =20,minsplite=3, minbucket=6
    tree<-prune(dtree,cp=dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"])
    probsFitOut <- list(predMethod = "CART", maxRating = maxRating, lst = tree, newXs = newXs)
  }
  
  class(probsFitOut) <- 'recProbs'
  return(probsFitOut)
}
    
 
  
predict.recProbs <- function(probsFitOut,newXs){  
  #CART
  
  #ct_pred1 <- predict(ctout,test,type="prob")
  #ct_node <- predict(ctout,test,type="node")
  #ct_pred2 <- predict(ctout,test)
  #ct_expect <- as.matrix(ct_pred1)%*%c(1,2,3,4,5)
  #ct_mape <- mean(abs(ct_expect-test[,3]))
  #ct_acc2 <- count(ct_pred2==test[,3])[2,2]/nrow(test)
  #plot(ctout)
  #confusionMatrix(table(ct_pred2,test$ratings))
  
  #RPART
  
  #printcp(dtree)
  #plotcp(dtree)
  #dt_pred2 <- predict(dtree,test)
  #dt_expect <- dt_pred2%*%c(1,2,3,4,5)
  #dt_mape <- mean(abs(dt_expect-test[,3]))
  #dt_predtree<-predict(dtree,test,type="class")
  #dt_acc <- count(dt_predtree==test[,3])[2,2]/nrow(test)
  #confusionMatrix(table(dt_predtree,test$ratings))
  
  
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
  return(t_pred2)

}

library(lme4)
data(InstEval)
dataIn <- InstEval[,c(1,2,7)]

#song <- read.csv('/Users/xianliu/ECS189/final\ project/songsDataset.csv', header=TRUE) 
probFitOut <- ratingProbsFit(dataIn = dataIn, maxRating = 5,predMethod = 'CART',embedMeans = TRUE)
prob <- predict.recProbs(probsFitOut = train, newXs = newXs)




#plot(data_mean$userid, data_mean$itemid, col=c("red","orange","yellow","green","blue")[data_mean$ratings])
#plot(train$userid, train$itemid, col=c("red","orange","yellow","green","blue")[train$ratings])
#plot(test$userid, test$itemid, col=c("red","orange","yellow","green","blue")[test$ratings])









