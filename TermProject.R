ratingProbsFit <- function(dataIn,maxRating,preMethod,embedMeans,specialArgs){
  source(./GLM.R)
  
  probsFitOut <- switch(preMethod, "glm" = buildGLM(dataIn,maxRating,embedMeans,specialArgs),
                        "knn" = (),"NMF" = (),"CART" = ())
  return(probsFitOut)
}

predict.recProbs <- function(probsFitOut,newXs){
# TODO: 1. Check no new user nor new item
#       2. Build the required output matrix, probably in subclasses.
#       3. Parse the preMethod from probsFitOut, making list??
  preds<-switch(predMehod, "glm" = predictGlm(probsFitOut,newXs),
                "knn" = (),"NMF" = (),"CART" = ())
  
  return(preds)
}
