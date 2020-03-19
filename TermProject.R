ratingProbsFit <- function(dataIn,maxRating,preMethod,embedMeans,specialArgs){
  if(preMehtod == "logit"){
   
  }else if(preMethod == "NMF"){
    if(embedMeans){
      stop("Error: invalid embedMean for NMF\n")  
    }
   
  }else if(preMehod == "kNN"){
    
  }else if(preMethod == "CART"){
    if(!embedMeans){
      stop("Error: invalid embedMean for CART\n")  
    }
  }
  
  return(probsFitOut)
}

predict.recProbs <- function(probsFitOut,newXs){
  # source: https://www.rdocumentation.org/packages/prob/versions/1.0-1/topics/setdiff
  isNewUser = setdiff(newXs$userID,probsFitOut$dataIn$userID)
  isNewItem = setdiff(newXs$itemID,probsFitOut$dataIn$itemID)
  
  if(!identical(isNewUser,integer(0)) || !identical(isNewItem,integer(0))){
    # source:https://stat.ethz.ch/R-manual/R-devel/library/base/html/stop.html
    stop("ERROR: NEW USER OR NEW ITEM\n") 
  }
  
  
  if(probsFitOut$preMehtod == "logit"){
   
  }else if(probsFitOut$preMehtod == "NMF"){
   
  }else if(probsFitOut$preMehtod == "kNN"){
    
  }else if(probsFitOut$preMehtod == "CART"){
    
  }
  
  return(preds)
}
