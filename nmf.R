nmf_model<-function(yourData,test,maxRating,rank){
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

  #test_num<-round(nrow(yourData)/5)
  #test<-sample(1:nrow(yourData),200)
  #train<-yourData[-test,]
  #test<-yourData[test,]

  train<-yourData
  
  result<-data.frame(c(test[,1]),c(test[,2]))
  for(i in (1:maxRating)){
    train_set=data_memory(user_index = train$userID,
                          item_index = train$itemID,
                          rating = train[,i+3],
                          index1 = TRUE)
    opts=r$tune(train_set,opts=list(dim=rank,niter=100,nmf=TRUE))
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
  out<-list(result=result,test=test)
  return(out)
}

#InstEval data

library(lme4)
data(InstEval)
dataset1<-InstEval[,c(1,2,7)]
names(dataset1)<-c("userID","itemID","rating")
#sample_index<-sample(1:nrow(dataset1),1000)
#sample_data<-dataset1[sample_index,]
#sample_data<-dataset1[1:1000,]
#out<-nmf_model(sample_data,5)
out<-nmf_model(dataset1,5)

result<-out$result
test<-out$test
#MAPE
sum<-0
for(i in (3:ncol(result))){
  sum<-sum+(i-2)*result[i]
}
mean(abs(unlist(sum)-test$rating)) #0.92

#accuracy
pred_rating<-max.col(result[,3:7])
table(pred_rating==test$rating)  #29%

#song data
dataset2<-read.csv("Documents/2020winter/ecs189g/project/songsDataset.csv")
names(dataset2)<-c("userID","itemID","rating")
sample_index2<-sample(1:nrow(dataset2),10000)
sample_data2<-dataset2[sample_index2,]
#sample_data2<-dataset2[1:1000,]
out2<-nmf_model(sample_data2,5)
result<-out2$result
test<-out2$test

#MAPE
sum<-0
for(i in (3:ncol(result))){
  sum<-sum+(i-2)*result[i]
}
mean(abs(unlist(sum)-test$rating)) #1.51

#accuracy
pred_rating<-max.col(result[,3:7])
table(pred_rating==test$rating)  #39%
