nmf_model<-function(yourData,maxRating){
  library(recosystem)
  library(regtools)
  col_name<-c('rating1','rating2','rating3','rating4','rating5')
  for(i in (1:maxRating)){
    yourData[,i+3]<-0
    names(yourData)[i+3]<-col_name[i]
  }
  
  for(i in (1:nrow(yourData))){
    yourData[i,yourData[i,3]+3]=1
  }
  
  r=Reco()
  test<-sample(1:nrow(yourData),200)
  train<-yourData[-test,]
  test<-yourData[test,]
  
  result<-data.frame(c(train[,1]),c(train[,2]))
  for(i in (1:maxRating)){
    train_set=data_memory(user_index = train$userID,
                          item_index = train$itemID,
                          rating = train[,i+3],
                          index1 = TRUE)
    opts=r$tune(train_set,opts=list(dim=100,niter=10,nmf=TRUE))
    r$train(train_set,opts=c(opts$min,nthread=1,niter=10,nmf=TRUE))
    
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
  
  return(result)
}


