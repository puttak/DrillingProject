DrillingFileReader = function(wellName)
{
  
     sqlQry = paste0("Select * From ", wellName, " WHERE [Hole Depth] > 0")
     
    
     cn <- odbcDriverConnect(connection="Driver={Sql Server};server=dev-ai-04\\bi;database=PL_SampleData;trusted_connection=yes;")
     df= sqlQuery(cn, sqlQry)
     close(cn)
     
     ## Remove spaces and weird characters from column names 
     names(df) = gsub(" ","",names(df))
     names(df) = gsub("-","",names(df))
     names(df) = gsub("/","",names(df))
     names(df) = gsub("\\(","",names(df))
     names(df) = gsub("\\)","",names(df))
     names(df) = gsub("#","",names(df))
     
     ## change all column types to numeric
     colIndex = c(2:ncol(df))
     df[,colIndex] = apply(df[,colIndex],2,function(x) as.numeric(as.character(x)))

     df
     
  
  
}