fiveFold<-function(yourData){
  #randomly shuffle the data
  yourData<-yourData[sample(nrow(yourData)),]
  
  #create 5 equally size folds
  folds<-cut(seq(1,nrow(yourData)),breaks = 5,labels = FALSE)
  
  #perform 10 fold cross validation
  
  for(i in 1:5){
    #segement your data by fold using the which() function
    testIndexes<=which(folds==i,arr.ind = TRUE)
    testData<-yourData[testIndexes, ]
    trainData<-yourData[-testIndexes, ]
    
    #Use the test and train data partitions whatever you desire...
  }
}
