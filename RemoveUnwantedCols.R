
## This funciton removes any column with variance equal to zero
DetectConstantCols = function(dataSource)
{
  ## Remove constant columns
  
  varVector = apply(dataSource,2,function(x) var(x, na.rm = TRUE))
  constantCols = names(which(varVector==0))
  constantCols
  
  
}


RemoveColsList = function(dataSource, unwantedCols)
{
  
  
  
  if(!(all( unwantedCols %in% names(dataSource))))
  {
    warning("Not all columns listed exists in the data frame")
    notExistCols = unwantedCols[!(unwantedCols %in% names(dataSource))]
    for(i in 1:length(notExistCols))
    {
      warningMsg = paste0("Column ", notExistCols[i], " doesn't exist in the input dataframe")
      warning(warningMsg)
    }
    
  }
  dataSource = dataSource[,-which(names(dataSource) %in% unwantedCols)]
  dataSource
  
}




DetectCorrCols = function(dataSource, mycutoff)
{
  
  set.seed(1234)
  dsCorr = cor(dataSource)
  set.seed(1234)
  dsHighCorr = findCorrelation(dsCorr, cutoff = mycutoff, names = TRUE, exact = TRUE)
  dsHighCorr
  
}



DetectColsWithMissingValues = function(dataSource, missValuesPercent)
{
  
  #dataSource[dataSource < 0] = -999.25
  
  
  missValues = apply(dataSource, 2, function(x){sum(x==-999.25)})
  missValuesPercentage = (missValues / nrow(dataSource))* 100
  missVariables = names(missValuesPercentage[missValuesPercentage>= missValuesPercent])
  missVariables
  
  
  
}







