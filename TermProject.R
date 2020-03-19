ratingProbsFit <- function(dataIn,maxRating,preMethod,embedMeans,specialArgs){
  source(./GLM.R)
  
  probsFitOut <- switch(preMethod, "logit" = buildGLM(dataIn,maxRating,embedMeans,specialArgs),
                        "knn" = (),"NMF" = (),"CART" = ())
  return(probsFitOut)
}

predict.recProbs <- function(probsFitOut,newXs){
  # source: https://www.rdocumentation.org/packages/prob/versions/1.0-1/topics/setdiff
  isNewUser = setdiff(newXs$userID,probsFitOut$dataIN$userID)
  isNewItem = setdiff(newXs$itemID,probsFitOut$dataIN$itemID)
  
  if(isNewUser != 0 || isNewItem != 0){
    # source:https://stat.ethz.ch/R-manual/R-devel/library/base/html/stop.html
    stop("ERROR: NEW USER OR NEW ITEM\n") 
  }
  
  
  preds<-switch(predMehod, "logit" = predictGlm(probsFitOut,newXs),
                "knn" = (),"NMF" = (),"CART" = ())
  
  return(preds)
}
