DrillingClassLabel = function(skipHours, eventHours, dataSource)
{
  
  maxTime = max(dataSource$DateTime)
  maxTimeMinusSkipHours = maxTime
  hour(maxTimeMinusSkipHours) = hour(maxTimeMinusSkipHours) - skipHours
  maxTimeMinuseventHours = maxTime
  hour(maxTimeMinuseventHours) = hour(maxTimeMinuseventHours) - skipHours - eventHours
  
  dataSource = dataSource[dataSource$DateTime < maxTimeMinusSkipHours, ]
  
  
  dataSource$EventClass = ifelse(dataSource$DateTime<= maxTimeMinuseventHours, 0,1)
  yesFreq =  sum(dataSource$EventClass == 1) / sum(dataSource$EventClass == 0)
  
  if(yesFreq * 100 < 10)
  {
    warning(paste0("The yes to no frequency is very low. It is set to ", yesFreq * 100, "%" ))
  }
  
  dataSource
  
}