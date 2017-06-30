library(RODBC); 
library(ggplot2);
library(data.table);
library(googleVis);
library(reshape2);
library(dplyr);
library(lubridate);
library(Boruta);
library(caret);
library(mlbench);
library(e1071);
library(rattle);
library(rpart);
library(rpart.plot);
library(randomForest);
library(ROCR);
library(glmnet);
library(ROSE);
library(corrplot);

setwd("C:\\Development\\R Projects\\DrillingDataAnalysis")

source("DrillingFileReader.R")
source("RemoveUnwantedRows.R")
source("NormalizeAccumCols.R")
source("DrillingClassLabel.R")
source("RemoveUnwantedCols.R")
source("CalculateVarImport.R")
source("ReturnConfusionMatrix.R")
source("FacetWrapLineChart.R")

removedColumnsList = list()

## Read csv files
df_original_31862 = DrillingFileReader(wellName = "well_31862")
df_original_31943 = DrillingFileReader(wellName = "well_31943")
df_original_31944 = DrillingFileReader(wellName = "well_31944")
df_original_31945 = DrillingFileReader(wellName = "well_31945")
df_original_31946 = DrillingFileReader(wellName = "well_31946")
df_original_31947 = DrillingFileReader(wellName = "well_31947")
df_original_31948 = DrillingFileReader(wellName = "well_31948")
df_original_32202 = DrillingFileReader(wellName = "well_32202")



## Remove constant columns
options(warn = -1)
constCols_31862 = DetectConstantCols(dataSource = df_original_31862)
constCols_31943 = DetectConstantCols(dataSource = df_original_31943)
constCols_31944 = DetectConstantCols(dataSource = df_original_31944)
constCols_31945 = DetectConstantCols(dataSource = df_original_31945)
constCols_31946 = DetectConstantCols(dataSource = df_original_31946)
constCols_31947 = DetectConstantCols(dataSource = df_original_31947)
constCols_31948 = DetectConstantCols(dataSource = df_original_31948)
constCols_32202 = DetectConstantCols(dataSource = df_original_32202)
options(war = 0)

## Constant Columns from different wells
uniqueConstCols = unique(c(constCols_31862, constCols_31943, constCols_31944, constCols_31945, constCols_31946, constCols_31947, constCols_31948, constCols_32202))

## Cleanup data frames
df_0_31862 = RemoveColsList(df_original_31862,uniqueConstCols)
df_0_31943 = RemoveColsList(df_original_31943,uniqueConstCols)
df_0_31944 = RemoveColsList(df_original_31944,uniqueConstCols)
df_0_31945 = RemoveColsList(df_original_31945,uniqueConstCols)
df_0_31946 = RemoveColsList(df_original_31946,uniqueConstCols)
df_0_31947 = RemoveColsList(df_original_31947,uniqueConstCols)
df_0_31948 = RemoveColsList(df_original_31948,uniqueConstCols)
df_0_32202 = RemoveColsList(df_original_32202,uniqueConstCols)


dfRemConstCols = data.frame(FeatureName = uniqueConstCols, Reason = "Constant Column - Zero Variance", wellName = "All Wells", Version = "Main")


## Remove Columns with More than 100 Missing Values
missCols_31862 = DetectColsWithMissingValues(df_0_31862, missValuesPercent= 5)
missCols_31943 = DetectColsWithMissingValues(df_0_31943, missValuesPercent= 5)
missCols_31944 = DetectColsWithMissingValues(df_0_31944, missValuesPercent= 5)
missCols_31945 = DetectColsWithMissingValues(df_0_31945, missValuesPercent= 5)
missCols_31946 = DetectColsWithMissingValues(df_0_31946, missValuesPercent= 5)
missCols_31947 = DetectColsWithMissingValues(df_0_31947, missValuesPercent= 5)
missCols_31948 = DetectColsWithMissingValues(df_0_31948, missValuesPercent= 5)
missCols_32202 = DetectColsWithMissingValues(df_0_32202, missValuesPercent= 5)


uniqueMissCols = unique(c(missCols_31862, missCols_31943, missCols_31944, missCols_31945, missCols_31946, missCols_31947, missCols_31948, missCols_32202))

## cleanup of data frames
df_1_31862 = RemoveColsList(df_0_31862, uniqueMissCols)
df_1_31943 = RemoveColsList(df_0_31943, uniqueMissCols)
df_1_31944 = RemoveColsList(df_0_31944, uniqueMissCols)
df_1_31945 = RemoveColsList(df_0_31945, uniqueMissCols)
df_1_31946 = RemoveColsList(df_0_31946, uniqueMissCols)
df_1_31947 = RemoveColsList(df_0_31947, uniqueMissCols)
df_1_31948 = RemoveColsList(df_0_31948, uniqueMissCols)
df_1_32202 = RemoveColsList(df_0_32202, uniqueMissCols)


dfRemMissCols = data.frame(FeatureName = uniqueMissCols, Reason = "Missing Columns - More than 5% of rows", wellName = "All Wells", version = "Main")



## Normalize Accum Columns
accumColToNormalize = c("HoleDepth","AccumRotations","BitHours","BitPosition","BitTVD","CirculatingHrs","CutandSlip","GammaDepth","KellyDown","PipeLength","PipeLengthChange",
                   "QLStartDepth","SoftSpeedStringLength","StringLength","StrksFill","TVD","QLStrokesToSurface","QLStksBtUp","HookLoad")

accumColToNormalize = accumColToNormalize[-(which(accumColToNormalize %in% c(uniqueMissCols, uniqueConstCols)))]

df_1_31862 = NormalizeAccumCols(df_1_31862, accumColToNormalize)
df_1_31943 = NormalizeAccumCols(df_1_31943, accumColToNormalize)
df_1_31944 = NormalizeAccumCols(df_1_31944, accumColToNormalize)
df_1_31945 = NormalizeAccumCols(df_1_31945, accumColToNormalize)
df_1_31946 = NormalizeAccumCols(df_1_31946, accumColToNormalize)
df_1_31947 = NormalizeAccumCols(df_1_31947, accumColToNormalize)
df_1_31948 = NormalizeAccumCols(df_1_31948, accumColToNormalize)
df_1_32202 = NormalizeAccumCols(df_1_32202, accumColToNormalize)

## Remove Accum Columns
accumColToNormalize = accumColToNormalize[-which(accumColToNormalize %in% c("HoleDepth"))]
df_1_31862 = RemoveColsList(df_1_31862, accumColToNormalize)
df_1_31943 = RemoveColsList(df_1_31943, accumColToNormalize)
df_1_31944 = RemoveColsList(df_1_31944, accumColToNormalize)
df_1_31945 = RemoveColsList(df_1_31945, accumColToNormalize)
df_1_31946 = RemoveColsList(df_1_31946, accumColToNormalize)
df_1_31947 = RemoveColsList(df_1_31947, accumColToNormalize)
df_1_31948 = RemoveColsList(df_1_31948, accumColToNormalize)
df_1_32202 = RemoveColsList(df_1_32202, accumColToNormalize)

dfRemAccumtCols = data.frame(FeatureName = accumColToNormalize, Reason = "Accum Columns Replaced with Diff Columns", wellName = "All Wells", Version = "Main")

## Remove unwanted Columns by Kurt
unwantedColsKurt =  c("ADTargetWeight", "ADTargetBitWeight", "ADTargetDiffPress","ADTargetROP","ADTargetTorque","PitVolume5","PitVolume4","PitVolume3","PitVolume2","PitVolume1")
df_1_31862 = RemoveColsList(df_1_31862, unwantedColsKurt)
df_1_31943 = RemoveColsList(df_1_31943, unwantedColsKurt)
df_1_31944 = RemoveColsList(df_1_31944, unwantedColsKurt)
df_1_31945 = RemoveColsList(df_1_31945, unwantedColsKurt)
df_1_31946 = RemoveColsList(df_1_31946, unwantedColsKurt)
df_1_31947 = RemoveColsList(df_1_31947, unwantedColsKurt)
df_1_31948 = RemoveColsList(df_1_31948, unwantedColsKurt)
df_1_32202 = RemoveColsList(df_1_32202, unwantedColsKurt)


## Remove all 0 data frames
rm(df_0_31862)
rm(df_0_31943)
rm(df_0_31944)
rm(df_0_31945)
rm(df_0_31946)
rm(df_0_31947)
rm(df_0_31948)
rm(df_0_32202)

## Split experiments based on their bit change
df_1_31862_exp1 = df_1_31862[df_1_31862$HoleDepth>= 2296 & df_1_31862$HoleDepth<= 4258, ]
df_1_31943_exp1 = df_1_31943[df_1_31943$HoleDepth>= 458 & df_1_31943$HoleDepth<= 1626, ]
df_1_31943_exp2 = df_1_31943[df_1_31943$HoleDepth>= 2095 & df_1_31943$HoleDepth<= 3828, ]
df_1_31945_exp1 = df_1_31945[df_1_31945$HoleDepth>= 2278 & df_1_31945$HoleDepth<= 3311, ]
df_1_31946_exp1 = df_1_31946[df_1_31946$HoleDepth>= 2153 & df_1_31946$HoleDepth<= 3767, ]
df_1_32202_exp1 = df_1_32202[df_1_32202$HoleDepth>= 455 & df_1_32202$HoleDepth<=999, ]

dim(df_1_31862_exp1)
dim(df_1_31943_exp1)
dim(df_1_31943_exp2)
dim(df_1_31945_exp1)
dim(df_1_31946_exp1)
dim(df_1_32202_exp1)

## combine the splitted data frames
 facetWrapData = rbind(cbind(melt(df_1_31862_exp1,id.vars = c("DateTime")),Depth = "Well_31862_exp1"), 
                       cbind(melt(df_1_31943_exp1,id.vars = c("DateTime")),Depth = "Well_31943_exp1"), 
                       cbind(melt(df_1_31943_exp2,id.vars = c("DateTime")),Depth = "Well_31943_exp2"), 
                       cbind(melt(df_1_31945_exp1,id.vars = c("DateTime")),Depth = "Well_31945_exp1"), 
                       cbind(melt(df_1_31946_exp1,id.vars = c("DateTime")),Depth = "Well_31946_exp1"),
                       cbind(melt(df_1_32202_exp1,id.vars = c("DateTime")),Depth = "Well_32202_exp1")
                       )
 
 
 ## See line charts
 folderName = "C:\\Development\\R Projects\\DrillingDataAnalysis\\FacetPlotsBeforeExploit"
 do.call(file.remove, list(list.files(folderName,full.names = TRUE)))
 
 myLinePlot = function(plotCol){
   fileName = paste0(folderName,"\\", plotCol,".png")
   
   p = FacetWrapLineChart(df = facetWrapData, variableName = plotCol)
   p
   ggsave(fileName, width = 20, height = 20)

 }
 
 colNames = colnames(df_1_31862_exp1)
 facetColNames = unique(colNames[!(colNames %in% c("DateTime"))])
 
 #facetColNames = c("ADBitWeight", "HoleDepth")
 lapply(facetColNames,myLinePlot)

 ## filtering based on exploitation
 df_1_31862_exp1 = df_1_31862_exp1[df_1_31862_exp1$DateTime>= "2017-01-04 00:00:00",]
 df_1_31943_exp1 = df_1_31943_exp1[df_1_31943_exp1$DateTime>= "2017-01-10 08:00:00", ]
 df_1_31943_exp2 = df_1_31943_exp2[df_1_31943_exp2$DateTime>= "2017-01-13 12:00:00" & df_1_31943_exp2$DateTime< "2017-01-16 00:00:00", ]
 df_1_31945_exp1 = df_1_31945_exp1[df_1_31945_exp1$DateTime>= "2017-01-31 07:00:00", ]
 df_1_31946_exp1 = df_1_31946_exp1[df_1_31946_exp1$DateTime>= "2017-02-09 00:00:00" & df_1_31946_exp1$ADBitWeight> -500, ]
 df_1_32202_exp1 = df_1_32202_exp1[df_1_32202_exp1$DateTime>= "2017-02-01 00:00:00", ]
 
## Clean unwanted Columns based on feedback and those that are not needed anymore
unwantedColsExploit = c("AccumTripOut" , "BHALength", "BitSize", "BitStatus", "DegasserTankS10DAQ", "HoleDepth", "Last24HrWear", "PumpSPM2","QLTimeToSurface","RigActivityCode", "SoftSpeedActive", "SoftSpeedEffectivePipeOuterDiameter","SoftSpeedPGain",        
                        "SoftSpeedStickSlipAnalyzerTime", "SoftSpeedStickSlipRawSeverity", "SoftSpeedTally1BHAID" ,"SoftSpeedTally1BHAlength" ,"SoftSpeedTally1BHAOD" ,"SoftSpeedTally2ID","SoftSpeedTally2length",          
                        "SoftSpeedTally2OD" ,"SoftSpeedTally3ID" ,"SoftSpeedTally3length","SoftSpeedTally3OD" ,"SoftSpeedTally4ID" , "SoftSpeedTally4length","SoftSpeedTally4OD"   , "SoftSpeedTDSHeartbeat" , "SoftSpeedTITime","StrksTotal"  , "TotalTripVolume", "TriggerHKLDStatus","TripTank","TripTankGL" , "WPDABitHHP" ,"WPDAStatus")



df_1_31862_exp1 = RemoveColsList(df_1_31862_exp1,unwantedColsExploit)
df_1_31943_exp1 = RemoveColsList(df_1_31943_exp1,unwantedColsExploit)
df_1_31943_exp2 = RemoveColsList(df_1_31943_exp2,unwantedColsExploit)
df_1_31945_exp1 = RemoveColsList(df_1_31945_exp1,unwantedColsExploit)
df_1_31946_exp1 = RemoveColsList(df_1_31946_exp1,unwantedColsExploit)
df_1_32202_exp1 = RemoveColsList(df_1_32202_exp1,unwantedColsExploit)


dfRemExploitCols = data.frame(FeatureName = unwantedColsExploit, Reason = "Remove Columns based on Exploitation", wellName = "All Wells", version = "Main")

## Plot again to make sure everything still looks good
facetWrapData2 = rbind(cbind(melt(df_1_31862_exp1,id.vars = c("DateTime")),Depth = "Well_31862_exp1"), 
                      cbind(melt(df_1_31943_exp1,id.vars = c("DateTime")),Depth = "Well_31943_exp1"), 
                      cbind(melt(df_1_31943_exp2,id.vars = c("DateTime")),Depth = "Well_31943_exp2"), 
                      cbind(melt(df_1_31945_exp1,id.vars = c("DateTime")),Depth = "Well_31945_exp1"), 
                      cbind(melt(df_1_31946_exp1,id.vars = c("DateTime")),Depth = "Well_31946_exp1"),
                      cbind(melt(df_1_32202_exp1,id.vars = c("DateTime")),Depth = "Well_32202_exp1"))

folderName = "C:\\Development\\R Projects\\DrillingDataAnalysis\\FacetPlotsAfterExploit"
do.call(file.remove, list(list.files(folderName,full.names = TRUE)))

myLinePlot = function(plotCol){
  fileName = paste0(folderName,"\\", plotCol,".png")
  
  p = FacetWrapLineChart(df = facetWrapData2, variableName = plotCol)
  p
  ggsave(fileName, width = 20, height = 20)
  
}

colNames = colnames(df_1_31862_exp1)
facetColNames = unique(colNames[!(colNames %in% c("DateTime"))])
#facetColNames = c("ADBitWeight", "HoleDepth")
lapply(facetColNames,myLinePlot)


## Need to use time to do the class labelling 
## Class labelling
df_1_31862_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_1_31862_exp1)
df_1_31943_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_1_31943_exp1)
df_1_31943_exp2 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_1_31943_exp2)
df_1_31945_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_1_31945_exp1)
df_1_31946_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_1_31946_exp1)
df_1_32202_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_1_32202_exp1)




## Plot corr plot and linearily
cor_31862_exp1 = cor(df_1_31862_exp1)
corrplot(cor_31862_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" )

cor_31943_exp1 = cor(df_1_31943_exp1)
corrplot(cor_31943_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" )

cor_31943_exp2 = cor(df_1_31943_exp2)
corrplot(cor_31943_exp2, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" )

cor_31945_exp1 = cor(df_1_31945_exp1)
corrplot(cor_31945_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" )

cor_31946_exp1 = cor(df_1_31946_exp1)
corrplot(cor_31946_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" )

cor_32202_exp1 = cor(df_1_32202_exp1)
corrplot(cor_32202_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" )

## Plot linear correlation
df_1_31862_exp1_melt = melt(df_1_31862_exp1, id.vars = "DateTime")
ggplot(data = df_1_31862, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))

df_1_31943_exp1_melt = melt(df_1_31943_exp1, id.vars = "DateTime")
ggplot(data = df_1_31943, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))

df_1_31945_exp1_melt = melt(df_1_31945_exp1, id.vars = "DateTime")
ggplot(data = df_1_31945, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))

df_1_31946_exp1_melt = melt(df_1_31946_exp1, id.vars = "DateTime")
ggplot(data = df_1_31946, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))

df_1_32202_exp1_melt = melt(df_1_32202_exp1, id.vars = "DateTime")
ggplot(data = df_1_32202, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))


## find correlated columns
df_1_31862_exp1_CorrCols = DetectCorrCols(dataSource = df_1_31862_exp1, mycutoff = 0.97)
df_1_31943_exp1_CorrCols = DetectCorrCols(dataSource = df_1_31943_exp1, mycutoff = 0.97)
df_1_31943_exp2_CorrCols = DetectCorrCols(dataSource = df_1_31943_exp2, mycutoff = 0.97)
df_1_31945_exp1_CorrCols = DetectCorrCols(dataSource = df_1_31945_exp1, mycutoff = 0.97)
df_1_31946_exp1_CorrCols = DetectCorrCols(dataSource = df_1_31946_exp1, mycutoff = 0.97)
df_1_32202_exp1_CorrCols = DetectCorrCols(dataSource = df_1_32202_exp1, mycutoff = 0.97)


## remove DateTime 
df_1_31862_exp1 = df_1_31862_exp1[,c(2:ncol(df_1_31862_exp1))]
df_1_31943_exp1 = df_1_31943_exp1[,c(2:ncol(df_1_31943_exp1))]
df_1_31943_exp2 = df_1_31943_exp2[,c(2:ncol(df_1_31943_exp2))]
df_1_31945_exp1 = df_1_31945_exp1[,c(2:ncol(df_1_31945_exp1))]
df_1_31946_exp1 = df_1_31946_exp1[,c(2:ncol(df_1_31946_exp1))]
df_1_32202_exp1 = df_1_32202_exp1[,c(2:ncol(df_1_32202_exp1))]


## find Importance data frame
## after this block all these tables must have the same dimensionality
df_1_31862_exp1_Import = CalculateVarImport(dataSource = df_1_31862_exp1)
df_1_31862_exp1_ImportVars = FindImportantVariables(importanceSource = df_1_31862_exp1_Import, neededVariables = 25)
df_1_31862_exp1 = KeepImportantVariables(dataSource = df_1_31862_exp1, df_1_31862_exp1_ImportVars)


df_1_31943_exp1_Import = CalculateVarImport(dataSource = df_1_31943_exp1)
df_1_31943_exp1_ImportVars = FindImportantVariables(importanceSource = df_1_31943_exp1_Import, neededVariables = 25)
df_1_31943_exp1 = KeepImportantVariables(dataSource = df_1_31943_exp1, df_1_31943_exp1_ImportVars)

df_1_31943_exp2_Import = CalculateVarImport(dataSource = df_1_31943_exp2)
df_1_31943_exp2_ImportVars = FindImportantVariables(importanceSource = df_1_31943_exp2_Import, neededVariables = 25)
df_1_31943_exp2 = KeepImportantVariables(dataSource = df_1_31943_exp2, df_1_31943_exp2_ImportVars)

df_1_31945_exp1_Import = CalculateVarImport(dataSource = df_1_31945_exp1)
df_1_31945_exp1_ImportVars = FindImportantVariables(importanceSource = df_1_31945_exp1_Import, neededVariables = 25)
df_1_31945_exp1 = KeepImportantVariables(dataSource = df_1_31945_exp1, df_1_31945_exp1_ImportVars)

df_1_31946_exp1_Import = CalculateVarImport(dataSource = df_1_31946_exp1)
df_1_31946_exp1_ImportVars = FindImportantVariables(importanceSource = df_1_31946_exp1_Import, neededVariables = 25)
df_1_31946_exp1 = KeepImportantVariables(dataSource = df_1_31946_exp1, df_1_31946_exp1_ImportVars)

df_1_32202_exp1_Import = CalculateVarImport(dataSource = df_1_32202_exp1)
df_1_32202_exp1_ImportVars = FindImportantVariables(importanceSource = df_1_32202_exp1_Import, neededVariables = 25)
df_1_32202_exp1 = KeepImportantVariables(dataSource = df_1_32202_exp1, df_1_32202_exp1_ImportVars)






