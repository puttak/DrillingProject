
RemoveRowsWithMissingValues = function(dataSource)
{
  missRows = apply(dataSource,1,function(x){sum(x==-999.25)})
  ind = as.integer(names(missRows[missRows>0]))
  dataSource = dataSource[-c(ind),]
  
  dataSource
}
