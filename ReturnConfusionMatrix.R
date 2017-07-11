ReturnConfusionMatrix = function(myModel, myTesting)
{
  logpredictions = predict(myModel, newdata =  myTesting, type = 'prob')
  predictionsBinary = ifelse(logpredictions["YES"] > 0.7, "YES", "NO")
  cfm = confusionMatrix(predictionsBinary, myTesting$EventClass, positive = "YES")
  cfm
  
}