NormalizeAccumCols = function(dataSource, colsToNormalize)
{
 
  
  for(i in 1:length(colsToNormalize))
  {
    
    neededCol = colsToNormalize[i]
    dataSource$Diff = dataSource[,which(names(dataSource) %in% neededCol)] - shift(dataSource[,which(names(dataSource) %in% neededCol)], 1L, type = "lag", fill = 0)
    colnames(dataSource)[ncol(dataSource)] = paste0(neededCol,"Diff")
    dataSource = dataSource[2:nrow(dataSource),]
    dataSource
  
  }
  dataSource
  
}