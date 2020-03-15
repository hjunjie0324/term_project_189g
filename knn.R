
library(knnflex)

udummy <- data.frame(factorToDummies(dataIn$userID,'user'))
idummy <- data.frame(factorToDummies(dataIn$itemID,'item'))
x <- cbind(udummy,idummy)
num <- 2000
xsample <- sample(1:nrow(x),num)
xtest <- x[xsample,]
kdist <- knn.dist(xtest)

cl <- dataIn$rating[xsample]
cltrn <- cl[1:(num*0.8)]
cltst <- cl[(num*0.8+1):num]

pred <- knn.predict(1:(num*0.8),(num*0.8+1):num, cltrn, kdist, k=3)

knnout <- knn.probability(1:(num*0.8),(num*0.8+1):num, cltrn, kdist, k=3)
knnout <- data.frame(t(knnout))
colnames(knnout) <- c(1,2,3,4,5)
