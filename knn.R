
library(knnflex)
num <- 3000
xsample <- sample(1:nrow(x),num)
xtest <- x[xsample,]
# 5-fold cross validation
xtest<-xtest[sample(nrow(xtest)),]

#create 5 equally size folds
folds<-cut(seq(1,nrow(xtest)),breaks = 5,labels = FALSE)
err <- rep(0,5)
acc <- rep(0,5)

for(i in 1:5){
  #segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind = TRUE)
  testData <- xtest[testIndexes, ]
  trainData <- xtest[-testIndexes, ]
  data <- rbind(trainData,testData)
  kdist <- knn.dist(data)
  cl <- dataIn$rating[xsample]
  cltrn <- cl[-testIndexes]
  cltst <- cl[testIndexes]
  pred <- knn.predict(1:(num*0.8),(num*0.8+1):num, cltrn, kdist, k=i+1)
  
  knnout <- knn.probability(1:(num*0.8),(num*0.8+1):num, cltrn, kdist, k=i+1)
  knnout <- data.frame(t(knnout))
  colnames(knnout) <- c(1,2,3,4,5)
  
  err[i] <- mean(abs(pred-cltst))
  preds <- round(pred)
  acc[i] <- sum(preds==cltst)/num*5
}

