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
library(dplyr);

setwd("C:\\Development\\R Projects\\MachineLearningProjects\\DrillingProject")

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
df_original_32202 = DrillingFileReader(wellName = "well_32202")



## Split experiments based on their bit change
df_31862_exp1 = filter(df_original_31862, HoleDepth>= 2296 & HoleDepth<= 4258)
dim(df_31862_exp1)
df_31943_exp1 = filter(df_original_31943, HoleDepth>= 458 & HoleDepth<= 1626)
dim(df_31943_exp1)
df_31943_exp2 = filter(df_original_31943, HoleDepth>= 2095 & HoleDepth<= 3828)
dim(df_31943_exp2)
df_31945_exp1 = filter(df_original_31945, HoleDepth>= 2278 & HoleDepth<= 3311)
dim(df_31945_exp1)
df_31946_exp1 = filter(df_original_31946, HoleDepth>= 2153 & HoleDepth<= 3767)
dim(df_31946_exp1)
df_32202_exp1 = filter(df_original_32202, HoleDepth>= 455 & HoleDepth<= 999)
dim(df_32202_exp1)

## remove original data frames 
rm(list = ls(pattern = "original"))

## Remove rows where HoleDepth is within 10cm of BitPosition
df_31862_exp1 = RemoveInActivePeriod(df_31862_exp1)
dim(df_31862_exp1)
df_31943_exp1 = RemoveInActivePeriod(df_31943_exp1)
dim(df_31943_exp1)
df_31943_exp2 = RemoveInActivePeriod(df_31943_exp2)
dim(df_31943_exp2)
df_31945_exp1 = RemoveInActivePeriod(df_31945_exp1)
dim(df_31945_exp1)
df_31946_exp1 = RemoveInActivePeriod(df_31946_exp1)
dim(df_31946_exp1)
df_32202_exp1 = RemoveInActivePeriod(df_32202_exp1)
dim(df_32202_exp1)

## Remove constant columns
options(warn = -1)
constCols_31862_exp1 = DetectConstantCols(dataSource = df_31862_exp1)
constCols_31943_exp1 = DetectConstantCols(dataSource = df_31943_exp1)
constCols_31943_exp2 = DetectConstantCols(dataSource = df_31943_exp2)
constCols_31945_exp1 = DetectConstantCols(dataSource = df_31945_exp1)
constCols_31946_exp1 = DetectConstantCols(dataSource = df_31946_exp1)
constCols_32202_exp1 = DetectConstantCols(dataSource = df_32202_exp1)
options(war = 0)

## Constant Columns from different wells
uniqueConstCols = unique(c(constCols_31862_exp1, constCols_31943_exp1,constCols_31943_exp2,
                            constCols_31945_exp1, constCols_31946_exp1, constCols_32202_exp1))

## Cleanup data frames
df_31862_exp1 = RemoveColsList(df_31862_exp1,uniqueConstCols)
df_31943_exp1 = RemoveColsList(df_31943_exp1,uniqueConstCols)
df_31943_exp2 = RemoveColsList(df_31943_exp2,uniqueConstCols)
df_31945_exp1 = RemoveColsList(df_31945_exp1,uniqueConstCols)
df_31946_exp1 = RemoveColsList(df_31946_exp1,uniqueConstCols)
df_32202_exp1 = RemoveColsList(df_32202_exp1,uniqueConstCols)

dfRemConstCols = data.frame(FeatureName = uniqueConstCols, Reason = "Constant Column - Zero Variance", WellName = "All Wells", Version = "Main")


## Remove Columns with More than 5% Missing Values
missCols_31862_exp1 = DetectColsWithMissingValues(df_31862_exp1, missValuesPercent= 5)
missCols_31943_exp1 = DetectColsWithMissingValues(df_31943_exp1, missValuesPercent= 5)
missCols_31943_exp2 = DetectColsWithMissingValues(df_31943_exp2, missValuesPercent= 5)
missCols_31945_exp1 = DetectColsWithMissingValues(df_31945_exp1, missValuesPercent= 5)
missCols_31946_exp1 = DetectColsWithMissingValues(df_31946_exp1, missValuesPercent= 5)
missCols_32202_exp1 = DetectColsWithMissingValues(df_32202_exp1, missValuesPercent= 5)

## exclude the results from 32202 because we will not run the experiment for it
uniqueMissCols = unique(c(missCols_31862_exp1, missCols_31943_exp1, missCols_31943_exp2, missCols_31945_exp1, missCols_31946_exp1, missCols_32202_exp1))

## cleanup of data frames
df_31862_exp1 = RemoveColsList(df_31862_exp1, uniqueMissCols)
df_31943_exp1 = RemoveColsList(df_31943_exp1, uniqueMissCols)
df_31943_exp2 = RemoveColsList(df_31943_exp2, uniqueMissCols)
df_31945_exp1 = RemoveColsList(df_31945_exp1, uniqueMissCols)
df_31946_exp1 = RemoveColsList(df_31946_exp1, uniqueMissCols)
df_32202_exp1 = RemoveColsList(df_32202_exp1, uniqueMissCols)


dfRemMissCols = data.frame(FeatureName = uniqueMissCols, Reason = "Missing Columns - More than 5% of rows", WellName = "All Wells", Version = "Main")



## Normalize Accum Columns
accumColToNormalize = c("HoleDepth","AccumRotations","BitHours","BitPosition","BitTVD","CirculatingHrs","CutandSlip","GammaDepth","KellyDown","PipeLength","PipeLengthChange",
                        "QLStartDepth","SoftSpeedStringLength","StringLength","StrksFill","TVD","QLStrokesToSurface","QLStksBtUp","HookLoad", "PumpPressure",
                        "ReturnsDepthRealtime")

MissConsCols = unique(c(uniqueMissCols, uniqueConstCols))

accumColToNormalize = accumColToNormalize[!(accumColToNormalize %in% MissConsCols)]

df_31862_exp1 = NormalizeAccumCols(df_31862_exp1, accumColToNormalize)
df_31943_exp1 = NormalizeAccumCols(df_31943_exp1, accumColToNormalize)
df_31943_exp2 = NormalizeAccumCols(df_31943_exp2, accumColToNormalize)
df_31945_exp1 = NormalizeAccumCols(df_31945_exp1, accumColToNormalize)
df_31946_exp1 = NormalizeAccumCols(df_31946_exp1, accumColToNormalize)
df_32202_exp1 = NormalizeAccumCols(df_32202_exp1, accumColToNormalize)

## Remove Accum Columns
accumColToNormalize = accumColToNormalize[!(accumColToNormalize %in% c("HoleDepth"))]
df_31862_exp1 = RemoveColsList(df_31862_exp1, accumColToNormalize)
df_31943_exp1 = RemoveColsList(df_31943_exp1, accumColToNormalize)
df_31943_exp2 = RemoveColsList(df_31943_exp2, accumColToNormalize)
df_31945_exp1 = RemoveColsList(df_31945_exp1, accumColToNormalize)
df_31946_exp1 = RemoveColsList(df_31946_exp1, accumColToNormalize)
df_32202_exp1 = RemoveColsList(df_32202_exp1, accumColToNormalize)

dfRemAccumtCols = data.frame(FeatureName = accumColToNormalize, Reason = "Accum Columns Replaced with Diff Columns", WellName = "All Wells", Version = "Main")

## Remove unwanted Columns by Kurt
unwantedColsKurt =  c("ADTargetWeight", "ADTargetBitWeight", "ADTargetDiffPress","ADTargetROP","ADTargetTorque","PitVolume5","PitVolume4","PitVolume3","PitVolume2","PitVolume1")
df_31862_exp1 = RemoveColsList(df_31862_exp1, unwantedColsKurt)
df_31943_exp1 = RemoveColsList(df_31943_exp1, unwantedColsKurt)
df_31943_exp2 = RemoveColsList(df_31943_exp2, unwantedColsKurt)
df_31945_exp1 = RemoveColsList(df_31945_exp1, unwantedColsKurt)
df_31946_exp1 = RemoveColsList(df_31946_exp1, unwantedColsKurt)
df_32202_exp1 = RemoveColsList(df_32202_exp1, unwantedColsKurt)


dim(df_31862_exp1)
dim(df_31943_exp1)
dim(df_31943_exp2)
dim(df_31945_exp1)
dim(df_31946_exp1)
dim(df_32202_exp1)

## combine the splitted data frames
facetWrapData = rbind(cbind(melt(df_31862_exp1,id.vars = c("DateTime")),Depth = "Well_31862_exp1"), 
                      cbind(melt(df_31943_exp1,id.vars = c("DateTime")),Depth = "Well_31943_exp1"), 
                      cbind(melt(df_31943_exp2,id.vars = c("DateTime")),Depth = "Well_31943_exp2"), 
                      cbind(melt(df_31945_exp1,id.vars = c("DateTime")),Depth = "Well_31945_exp1"), 
                      cbind(melt(df_31946_exp1,id.vars = c("DateTime")),Depth = "Well_31946_exp1"),
                      cbind(melt(df_32202_exp1,id.vars = c("DateTime")),Depth = "Well_32202_exp1")
)


## See line charts
folderName = "C:\\Development\\R Projects\\MachineLearningProjects\\DrillingProject\\FacetPlotsBeforeExlpoit"
do.call(file.remove, list(list.files(folderName,full.names = TRUE)))

myLinePlot = function(plotCol){
  fileName = paste0(folderName,"\\", plotCol,".png")
  
  p = FacetWrapLineChart(df = facetWrapData, variableName = plotCol)
  p
  ggsave(fileName, height = 9, width = 12)
  
}

colNames = colnames(df_31862_exp1)
facetColNames = unique(colNames[!(colNames %in% c("DateTime"))])

#facetColNames = c("ADBitWeight", "HoleDepth")
lapply(facetColNames,myLinePlot)

## filtering based on exploitation
df_31862_exp1 = df_31862_exp1[df_31862_exp1$DateTime>= "2017-01-04 00:00:00",]
df_31943_exp1 = df_31943_exp1[df_31943_exp1$DateTime>= "2017-01-10 08:00:00", ]
df_31943_exp2 = df_31943_exp2[df_31943_exp2$DateTime>= "2017-01-13 12:00:00" & df_31943_exp2$DateTime< "2017-01-16 00:00:00", ]
df_31945_exp1 = df_31945_exp1[df_31945_exp1$DateTime>= "2017-01-31 07:00:00", ]
df_31946_exp1 = df_31946_exp1[df_31946_exp1$DateTime>= "2017-02-09 00:00:00" & df_31946_exp1$ADBitWeight> -500, ]
df_32202_exp1 = df_32202_exp1[df_32202_exp1$DateTime>= "2017-02-26 02:00:00", ]

## Clean unwanted Columns based on feedback and those that are not needed anymore
unwantedColsExploit = c("AccumTripOut" , "BHALength", "BitSize", "BitStatus", "DegasserTankS10DAQ", "HoleDepth", "Last24HrWear", "PumpSPM2","QLTimeToSurface","RigActivityCode", "SoftSpeedActive", "SoftSpeedEffectivePipeOuterDiameter","SoftSpeedPGain",        
                        "SoftSpeedStickSlipAnalyzerTime", "SoftSpeedStickSlipRawSeverity", "SoftSpeedTally1BHAID" ,"SoftSpeedTally1BHAlength" ,"SoftSpeedTally1BHAOD" ,"SoftSpeedTally2ID","SoftSpeedTally2length",          
                        "SoftSpeedTally2OD" ,"SoftSpeedTally3ID" ,"SoftSpeedTally3length","SoftSpeedTally3OD" ,"SoftSpeedTally4ID" , "SoftSpeedTally4length","SoftSpeedTally4OD"   , "SoftSpeedTDSHeartbeat" , "SoftSpeedTITime","StrksTotal"  , "TotalTripVolume", "TriggerHKLDStatus","TripTank","TripTankGL" , "WPDABitHHP" ,"WPDAStatus",
                        "PumpSPM1", "AccumRotationsDiff","BitHoursDiff","BitPositionDiff","BitTVDDiff","CoManGL","CutandSlipDiff",
                        "DogLegSeverity","Flock1","Flock2","Flock3","GainLoss","GainLossSpare","KellyDownDiff","LstElemLen",
                        "LstJntTime","MudVolume","PillTank","PipeLengthChangeDiff","PipeLengthDiff","PTCorrection","QLStartDepthDiff",
                        "SlipStatus","SlipSwitch","SoftSpeedKpGain","SoftSpeedStringLengthDiff","StringLengthDiff",
                        "StrksFillDiff","TVDDiff")



df_31862_exp1 = RemoveColsList(df_31862_exp1,unwantedColsExploit)
df_31943_exp1 = RemoveColsList(df_31943_exp1,unwantedColsExploit)
df_31943_exp2 = RemoveColsList(df_31943_exp2,unwantedColsExploit)
df_31945_exp1 = RemoveColsList(df_31945_exp1,unwantedColsExploit)
df_31946_exp1 = RemoveColsList(df_31946_exp1,unwantedColsExploit)
df_32202_exp1 = RemoveColsList(df_32202_exp1,unwantedColsExploit)


dfRemExploitCols = data.frame(FeatureName = unwantedColsExploit, Reason = "Remove Columns based on Exploitation", WellName = "All Wells", Version = "Main")

## Plot again to make sure everything still looks good
facetWrapData2 = rbind(cbind(melt(df_31862_exp1,id.vars = c("DateTime")),Depth = "Well_31862_exp1"), 
                       cbind(melt(df_31943_exp1,id.vars = c("DateTime")),Depth = "Well_31943_exp1"), 
                       cbind(melt(df_31943_exp2,id.vars = c("DateTime")),Depth = "Well_31943_exp2"), 
                       cbind(melt(df_31945_exp1,id.vars = c("DateTime")),Depth = "Well_31945_exp1"), 
                       cbind(melt(df_31946_exp1,id.vars = c("DateTime")),Depth = "Well_31946_exp1"),
                       cbind(melt(df_32202_exp1,id.vars = c("DateTime")),Depth = "Well_32202_exp1"))

folderName = "C:\\Development\\R Projects\\MachineLearningProjects\\DrillingProject\\FacetPlotsAfterExploit"
do.call(file.remove, list(list.files(folderName,full.names = TRUE)))

myLinePlot = function(plotCol){
  fileName = paste0(folderName,"\\", plotCol,".png")
  
  p = FacetWrapLineChart(df = facetWrapData2, variableName = plotCol)
  p
  ggsave(fileName, width = 20, height = 20)
  
}

colNames = colnames(df_31862_exp1)
facetColNames = unique(colNames[!(colNames %in% c("DateTime"))])
#facetColNames = c("ADBitWeight", "HoleDepth")
lapply(facetColNames,myLinePlot)


## Need to use time to do the class labelling 
## Class labelling
df_31862_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_31862_exp1)
df_31943_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_31943_exp1)
df_31943_exp2 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_31943_exp2)
df_31945_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_31945_exp1)
df_31946_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_31946_exp1)
df_32202_exp1 = DrillingClassLabel(skipHours = 2, eventHours = 3, dataSource = df_32202_exp1)




## Plot corr plot and linearily
cor_31862_exp1 = cor(df_31862_exp1[,c(2:ncol(df_31862_exp1))])
corrplot(cor_31862_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" , title = "Correlation Matrix for Well_31862")

cor_31943_exp1 = cor(df_31943_exp1[,c(2:ncol(df_31943_exp1))])
corrplot(cor_31943_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust", title = "Correlation Matrix for Well_31943_exp1" )

cor_31943_exp2 = cor(df_31943_exp2[,c(2:ncol(df_31943_exp2))])
corrplot(cor_31943_exp2, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust", title = "Correlation Matrix for Well_31943_exp2" )

cor_31945_exp1 = cor(df_31945_exp1[,c(2:ncol(df_31945_exp1))])
corrplot(cor_31945_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" , title = "Correlation Matrix for Well_31945")

cor_31946_exp1 = cor(df_31946_exp1[,c(2:ncol(df_31946_exp1))])
corrplot(cor_31946_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" , title = "Correlation Matrix for Well_31946")

cor_32202_exp1 = cor(df_32202_exp1[,c(2:ncol(df_32202_exp1))])
corrplot(cor_32202_exp1, tl.cex = 0.7, method = "pie", type = "lower", bg = "white", diag = FALSE, tl.col = "blue", cl.ratio = 0.1,  order ="hclust" , title = "Correlation Matrix for Well_32202")

## Plot linear correlation
df_31862_exp1_melt = melt(df_31862_exp1, id.vars = "DateTime")
ggplot(data = df_31862_exp1_melt, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))
df_31862_LinearCorr = df_31862_exp1_melt[df_31862_exp1_melt$variable %in% c("PumpPressure", "DiffPress"),]
ggplot(data = df_31862_LinearCorr, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))

df_31943_exp1_melt = melt(df_31943_exp1, id.vars = "DateTime")
ggplot(data = df_31943_exp1_melt, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))


df_31943_exp2_melt = melt(df_31943_exp1, id.vars = "DateTime")
ggplot(data = df_31943_exp2_melt, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))


df_31945_exp1_melt = melt(df_31945_exp1, id.vars = "DateTime")
ggplot(data = df_31945_exp1_melt, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))

df_31946_exp1_melt = melt(df_31946_exp1, id.vars = "DateTime")
ggplot(data = df_31946_exp1_melt, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))

df_32202_exp1_melt = melt(df_32202_exp1, id.vars = "DateTime")
ggplot(data = df_32202_exp1_melt, aes(x = DateTime, y  = value)) + geom_line(aes(colour = variable))



## remove DateTime 
df_31862_exp1 = df_31862_exp1[,c(2:ncol(df_31862_exp1))]
df_31943_exp1 = df_31943_exp1[,c(2:ncol(df_31943_exp1))]
df_31943_exp2 = df_31943_exp2[,c(2:ncol(df_31943_exp2))]
df_31945_exp1 = df_31945_exp1[,c(2:ncol(df_31945_exp1))]
df_31946_exp1 = df_31946_exp1[,c(2:ncol(df_31946_exp1))]
df_32202_exp1 = df_32202_exp1[,c(2:ncol(df_32202_exp1))]

## Second round to remove constant columns
## Remove constant columns
options(warn = -1)
constCols_31862_exp1 = DetectConstantCols(dataSource = df_31862_exp1)
constCols_31943_exp1 = DetectConstantCols(dataSource = df_31943_exp1)
constCols_31943_exp2 = DetectConstantCols(dataSource = df_31943_exp2)
constCols_31945_exp1 = DetectConstantCols(dataSource = df_31945_exp1)
constCols_31946_exp1 = DetectConstantCols(dataSource = df_31946_exp1)
constCols_32202_exp1 = DetectConstantCols(dataSource = df_32202_exp1)
options(war = 0)

## Constant Columns from different wells
uniqueConstCols2 = unique(c(constCols_31862_exp1, constCols_31943_exp1,constCols_31943_exp2,
                           constCols_31945_exp1, constCols_31946_exp1, constCols_32202_exp1))

## Cleanup data frames
df_31862_exp1 = RemoveColsList(df_31862_exp1,uniqueConstCols2)
df_31943_exp1 = RemoveColsList(df_31943_exp1,uniqueConstCols2)
df_31943_exp2 = RemoveColsList(df_31943_exp2,uniqueConstCols2)
df_31945_exp1 = RemoveColsList(df_31945_exp1,uniqueConstCols2)
df_31946_exp1 = RemoveColsList(df_31946_exp1,uniqueConstCols2)
df_32202_exp1 = RemoveColsList(df_32202_exp1,uniqueConstCols2)

dfRemConstCols2 = data.frame(FeatureName = uniqueConstCols2, Reason = "Constant Column - Zero Variance - Round 2", WellName = "All Wells", Version = "Main")





## find correlated columns
corrCols_31862_exp1 = DetectCorrCols(dataSource = df_31862_exp1, mycutoff = 0.97)
corrCols_31943_exp1 = DetectCorrCols(dataSource = df_31943_exp1, mycutoff = 0.97)
corrCols_31943_exp2 = DetectCorrCols(dataSource = df_31943_exp2, mycutoff = 0.97)
corrCols_31945_exp1 = DetectCorrCols(dataSource = df_31945_exp1, mycutoff = 0.97)
corrCols_31946_exp1 = DetectCorrCols(dataSource = df_31946_exp1, mycutoff = 0.97)
corrCols_32202_exp1 = DetectCorrCols(dataSource = df_32202_exp1, mycutoff = 0.97)

corrColsCommon = intersect(intersect(intersect(intersect(intersect(corrCols_31862_exp1,corrCols_31943_exp2),corrCols_31945_exp1),corrCols_31946_exp1),corrCols_31943_exp1),corrCols_32202_exp1)


df_31862_exp1 = RemoveColsList(df_31862_exp1,corrColsCommon)
df_31943_exp1 = RemoveColsList(df_31943_exp1,corrColsCommon)
df_31943_exp2 = RemoveColsList(df_31943_exp2,corrColsCommon)
df_31945_exp1 = RemoveColsList(df_31945_exp1,corrColsCommon)
df_31946_exp1 = RemoveColsList(df_31946_exp1,corrColsCommon)
df_32202_exp1 = RemoveColsList(df_32202_exp1,corrColsCommon)

dfRemCorrCols = data.frame(FeatureName = corrColsCommon, Reason = "Correlated Columns more than 0.97", WellName = "All Wells", Version = "Main")


## find Importance data frame
## after this block all these tables must have the same dimensionality
df_31862_exp1_Import = CalculateVarImport(dataSource = df_31862_exp1)
df_31862_exp1_ImportVars = FindImportantVariables(importanceSource = df_31862_exp1_Import, neededVariables = 25)
df_31862_exp1 = KeepImportantVariables(dataSource = df_31862_exp1, df_31862_exp1_ImportVars)


df_31943_exp1_Import = CalculateVarImport(dataSource = df_31943_exp1)
df_31943_exp1_ImportVars = FindImportantVariables(importanceSource = df_31943_exp1_Import, neededVariables = 25)
df_31943_exp1 = KeepImportantVariables(dataSource = df_31943_exp1, df_31943_exp1_ImportVars)

df_31943_exp2_Import = CalculateVarImport(dataSource = df_31943_exp2)
df_31943_exp2_ImportVars = FindImportantVariables(importanceSource = df_31943_exp2_Import, neededVariables = 25)
df_31943_exp2 = KeepImportantVariables(dataSource = df_31943_exp2, df_31943_exp2_ImportVars)

df_31945_exp1_Import = CalculateVarImport(dataSource = df_31945_exp1)
df_31945_exp1_ImportVars = FindImportantVariables(importanceSource = df_31945_exp1_Import, neededVariables = 25)
df_31945_exp1 = KeepImportantVariables(dataSource = df_31945_exp1, df_31945_exp1_ImportVars)

df_31946_exp1_Import = CalculateVarImport(dataSource = df_31946_exp1)
df_31946_exp1_ImportVars = FindImportantVariables(importanceSource = df_31946_exp1_Import, neededVariables = 25)
df_31946_exp1 = KeepImportantVariables(dataSource = df_31946_exp1, df_31946_exp1_ImportVars)


df_32202_exp1_Import = CalculateVarImport(dataSource = df_32202_exp1)
df_32202_exp1_ImportVars = FindImportantVariables(importanceSource = df_32202_exp1_Import, neededVariables = 25)
df_32202_exp1 = KeepImportantVariables(dataSource = df_32202_exp1, df_32202_exp1_ImportVars)


df_Importance = rbind(cbind(df_31862_exp1_Import, WellName = "well_31862"), 
                      cbind(df_31943_exp1_Import, WellName = "well_31943_exp1"),
                      cbind(df_31943_exp2_Import, WellName = "well_31943_exp2"),
                      cbind(df_31945_exp1_Import, WellName = "well_31945"),
                      cbind(df_31946_exp1_Import, WellName = "well_31946"),
                      cbind(df_32202_exp1_Import, WellName = "well_32202")
                     )

df_Removed = rbind(dfRemAccumtCols, dfRemConstCols, dfRemExploitCols, dfRemMissCols,dfRemConstCols2,dfRemCorrCols)




## Write back needed objects

save(df_Importance, df_Removed, file = "TransformationSummaryFiles_V2.RData")
save(df_31862_exp1, df_31943_exp1, df_31943_exp2, df_31945_exp1, df_31946_exp1, df_32202_exp1, file="DrillingDataLastVersion_V2.RData")


