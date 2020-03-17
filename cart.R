library(lme4)
library(regtools)
library(rectools)
library(recosystem)
library(partykit)
library(rpart)


song <- read.csv('/Users/xianliu/ECS189/final\ project/songsDataset.csv') 
dataIn <- InstEval[,c(1:2,7)]
colnames(dataIn) <- c('userid','itemid','ratings')

#get the mean value for userid and itemid
userMeans <- tapply ( dataIn$ratings, dataIn$userid, mean )
itemMeans <- tapply ( dataIn$ratings, dataIn$itemid, mean )
data_mean <- dataIn
data_mean$userid <- userMeans [ dataIn$userid]
data_mean$itemid <- userMeans [ dataIn$itemid]



test_s<-sample(1:nrow(data_mean),5000)
test<-data_mean[test_s,]
train<-data_mean[-test_s,]



#CART


ctout <- ctree(as.factor(ratings)~.,data=train)
ct_pred1 <- predict(ctout,test[,-3],type="prob")
ct_pred2 <- predict(ctout,test[,-3])
ct_expect <- pred%*%c(1,2,3,4,5)
ct_mape <- mean(abs(ct_expect-test[,3]))
ct_acc1 <- count(round(ct_expect)==test[,3])[2,2]/nrow(test)
ct_acc2 <- count(ct_pred2==test[,3])[2,2]/nrow(test)






dtree <- rpart(ratings~.,data=train, method="class",parms=list(split="gini"))
printcp(dtree)
print(dtree)
tree<-prune(dtree,cp=dtree$cptable[which.min(dtree$cptable[,"xerror"]),"CP"])
printcp(tree)


rpart.plot(dtree,branch=1,type=2, fallen.leaves=T,cex=0.8, sub="no prune")
rpart.plot(tree,branch=1, type=4,fallen.leaves=T,cex=0.8, sub="prune")


pred2 <- predict(dtree,test[,-3])
expect <- pred2%*%c(1,2,3,4,5)


acc2 <- count(round(expect)==test[,3])[2,2]/nrow(test)
predtree<-predict(tree,test[,-3],type="class")
acc <- count(predtree==test[,3])[2,2]/nrow(test)















