CalculateVarImport = function(dataSource)
{

## Random forest variable importance
dataSource$EventClass = factor(dataSource$EventClass)
## Five folds cross validation
myControl = trainControl(method="cv", number= 10,  returnResamp = 'none')
set.seed(1234)
rfModel = train(EventClass ~ ., data = dataSource, method = "rf", trControl = myControl, importance = TRUE)
varImportance = varImp(rfModel, scale = FALSE)
dfImport = data.frame(rownames(varImportance$importance),varImportance$importance[1])
names(dfImport) = c("VariableName","VariableImportance")
dfImport = dfImport[order(dfImport$VariableImportance, decreasing  = TRUE),]
rownames(dfImport) = NULL
dfImport

}


FindImportantVariables = function(importanceSource, neededVariables)
{
  dfImport = importanceSource
  dtImport = data.table(dfImport)
  dtImport = dtImport[order(dtImport$VariableImportance, decreasing = TRUE ),]
  dfImport = data.frame(dtImport[,Index := 1:.N])
  topXImport = as.character(dfImport[dfImport$Index <= neededVariables,"VariableName"])
  
}

KeepImportantVariables = function(dataSource, topXImport) 
{
  dataSource = dataSource[,which(names(dataSource) %in% c(topXImport,"EventClass"))]
  dataSource
  
}