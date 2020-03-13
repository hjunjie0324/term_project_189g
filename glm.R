#source("GLM.R")

library(recosystem)
library(regtools)

#readFile <- fuction(fileName){
 # file <- read.csv(fileName)
  #colnames(file) <- c("userID", "itemID", "rating")
  #
  #user_mean <- tapply(file$rating, file$userID, mean)
  #item_mean <- tapply(file$rating, file$itemID, mean)
  
#  for(i in (1:nrow(file))){
 #   user <-file[i,2]
  #  item <-file[i,1]
   # um[i,1]<-user_mean[user]
    #im[i,1]<-item_mean[item]
  #}
  
#  file$userMean = um
 # file$itemMean = im
  
  #return(file)
#}

setwd('C:/omsi-master')

#file <- readFile('songsDataset')

file <- read.csv('songsDataset.csv')
colnames(file) <- c("userID", "itemID", "rating")

user_mean <- tapply(file$rating, file$userID, mean)
item_mean <- tapply(file$rating, file$itemID, mean)

#file$userID<-NA

#file$itemID<-NA
#cbind(file,userMean,itemMean)
#file$userMean = NA
#file$itemMean = im

for(i in (1:nrow(file))){
  user <-file[i,2]
  item <-file[i,1]
  um<-c(user_mean[user])
  im<-c(item_mean[item])
}

file$userMean <- um
file$itemMean <- im



file

file$userID<-as.factor(file$userID)
file$itemID<-as.factor(file$itemID)
file$userMean<-as.factor(file$userMean)
#file$userID<-as.factor(file$userID)


RFile <- file

test<-sample(1:nrow(RFile),5000)
utest<-RFile[test,]
utrain<-RFile[-test,]

glmout <- glm(rating ~ userID + itemID, data = utrain, family = binomial)

pred <- predict.glm(glmout,utest,type = "prob")

pred

errorLM<-MAPE(pred,utest$rating)
errorLM


