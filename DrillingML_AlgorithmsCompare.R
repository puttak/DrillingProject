# Evaluate Algorithms
myConfusionMatrixList = list()
myResults = list()
mydfNames = c("df_1_31862_exp1", "df_1_31943_exp1","df_1_31943_exp2","df_1_31945_exp1","df_1_31946_exp1")
mydfList = list()
mydfList[["df_1_31862_exp1"]] = df_1_31862_exp1
mydfList[["df_1_31943_exp1"]] = df_1_31943_exp1
mydfList[["df_1_31943_exp2"]] = df_1_31943_exp2
mydfList[["df_1_31945_exp1"]] = df_1_31945_exp1
mydfList[["df_1_31946_exp1"]] = df_1_31946_exp1

for(j in 1:length(mydfNames))
{



dfname = mydfNames[j]
df = mydfList[[dfname]]

print(paste0("working on well " , dfname))
print(Sys.time())

df$EventClass = ifelse(df$EventClass==0,"NO","YES")
inTrain = createDataPartition(df$EventClass,p=0.7, list = FALSE)
trainingDS = df[inTrain,]
testingDS = df[-inTrain,]
trainingDS$EventClass = factor(trainingDS$EventClass, levels = c("YES","NO"))
i = 1

  set.seed(i)
  df_under = downSample(x = trainingDS[,-ncol(trainingDS)], y = trainingDS$EventClass)
  colnames(df_under)[ncol(df_under)] = "EventClass"
  
  set.seed(i)
  df_over = upSample(x = trainingDS[,-ncol(trainingDS)], y = trainingDS$EventClass)
  colnames(df_over)[ncol(df_over)] = "EventClass"
  
  set.seed(i)
  df_rose = ROSE(EventClass ~ ., data = trainingDS)$data

  
  # 10-fold cross validation with 3 repeats
  control <- trainControl(method="cv", number=10,classProbs = TRUE, summaryFunction = twoClassSummary)
  metric <- "ROC"
  
  
  # LG
  set.seed(i)
  fit.glm.original <- train(EventClass~., data=trainingDS, method="glm", metric=metric, trControl=control, na.action=na.omit)
  # cfm.glm.original = ReturnConfusionMatrix(myModel = fit.glm.original, myTesting = testingDS)
  # cfmName.glm.original = paste0("cfm_glm_original_",i)
  # myConfusionMatrixList[[cfmName.glm.original]] =  cfm.glm.original
  
  set.seed(i)
  fit.glm.under <- train(EventClass~., data=df_under, method="glm", metric=metric, trControl=control, na.action=na.omit)
  # cfm.glm.under = ReturnConfusionMatrix(myModel = fit.glm.under, myTesting = testingDS)
  # cfmName.glm.under = paste0("cfm_glm_under_",i)
  # myConfusionMatrixList[[cfmName.glm.under]] =  cfm.glm.under
  
  set.seed(i)
  fit.glm.over <- train(EventClass~., data=df_over, method="glm", metric=metric, trControl=control, na.action=na.omit)
  # cfm.glm.over = ReturnConfusionMatrix(myModel = fit.glm.over, myTesting = testingDS)
  # cfmName.glm.over = paste0("cfm_glm_over_",i)
  # myConfusionMatrixList[[cfmName.glm.over]] =  cfm.glm.over
  
  print(paste0("Done glm for well " , dfname))
  print(Sys.time())
  
  # set.seed(i)
  # fit.glm.rose <- train(EventClass~., data=df_rose, method="glm", metric=metric, trControl=control, na.action=na.omit)
  # cfm.glm.rose = ReturnConfusionMatrix(myModel = fit.glm.rose, myTesting = testingDS)
  # cfmName.glm.rose = paste0("cfm_glm_rose_",i)
  # myConfusionMatrixList[[cfmName.glm.rose]] =  cfm.glm.rose
  
  # LDA
  set.seed(i)
  fit.lda.original <- train(EventClass~., data=trainingDS, method="lda", metric=metric, trControl=control, na.action=na.omit)
  # cfm.lda.original = ReturnConfusionMatrix(myModel = fit.lda.original, myTesting = testingDS)
  # cfmName.lda.original = paste0("cfm_lda_original_",i)
  # myConfusionMatrixList[[cfmName.lda.original]] =  cfm.lda.original
  # 
  set.seed(i)
  fit.lda.under <- train(EventClass~., data=df_under, method="lda", metric=metric, trControl=control, na.action=na.omit)
  # cfm.lda.under = ReturnConfusionMatrix(myModel = fit.lda.under, myTesting = testingDS)
  # cfmName.lda.under = paste0("cfm_lda_under_",i)
  # myConfusionMatrixList[[cfmName.lda.under]] =  cfm.lda.under
  # 
  set.seed(i)
  fit.lda.over <- train(EventClass~., data=df_over, method="lda", metric=metric, trControl=control, na.action=na.omit)
  # cfm.lda.over = ReturnConfusionMatrix(myModel = fit.lda.over, myTesting = testingDS)
  # cfmName.lda.over = paste0("cfm_lda_over_",i)
  # myConfusionMatrixList[[cfmName.lda.over]] =  cfm.lda.over
  # 
  print(paste0("Done lda for well " , dfname))
  print(Sys.time())
  # set.seed(i)
  # fit.lda.rose <- train(EventClass~., data=df_rose, method="lda", metric=metric, trControl=control, na.action=na.omit)
  # cfm.lda.rose = ReturnConfusionMatrix(myModel = fit.lda.rose, myTesting = testingDS)
  # cfmName.lda.rose = paste0("cfm_lda_rose_",i)
  # myConfusionMatrixList[[cfmName.lda.rose]] =  cfm.lda.rose
  
   
  # # GLMNET
  # set.seed(i)
  # fit.glmnet.original <- train(EventClass~., data=trainingDS, method="glmnet", metric=metric, trControl=control, na.action=na.omit)
  # # cfm.glmnet.original = ReturnConfusionMatrix(myModel = fit.glmnet.original, myTesting = testingDS)
  # # cfmName.glmnet.original = paste0("cfm_glmnet_original_",i)
  # # myConfusionMatrixList[[cfmName.glmnet.original]] =  cfm.glmnet.original
  # 
  # set.seed(i)
  # fit.glmnet.under <- train(EventClass~., data=df_under, method="glmnet", metric=metric, trControl=control, na.action=na.omit)
  # # cfm.glmnet.under = ReturnConfusionMatrix(myModel = fit.glmnet.under, myTesting = testingDS)
  # # cfmName.glmnet.under = paste0("cfm_glmnet_under_",i)
  # # myConfusionMatrixList[[cfmName.glmnet.original]] =  cfm.glmnet.under
  # 
  # 
  # set.seed(i)
  # fit.glmnet.over <- train(EventClass~., data=df_over, method="glmnet", metric=metric, trControl=control, na.action=na.omit)
  # cfm.glmnet.over = ReturnConfusionMatrix(myModel = fit.glmnet.over, myTesting = testingDS)
  # cfmName.glmnet.over = paste0("cfm_glmnet_over_",i)
  # myConfusionMatrixList[[cfmName.glmnet.original]] =  cfm.glmnet.over

  print(paste0("Done glmnet for well " , dfname))
  print(Sys.time())
  # set.seed(i)
  # fit.glmnet.rose <- train(EventClass~., data=df_rose, method="glmnet", metric=metric, trControl=control, na.action=na.omit)
  # cfm.glmnet.rose = ReturnConfusionMatrix(myModel = fit.glmnet.rose, myTesting = testingDS)
  # cfmName.glmnet.rose = paste0("cfm_glmnet_rose_",i)
  # myConfusionMatrixList[[cfmName.glmnet.original]] =  cfm.glmnet.rose

  # KNN
  set.seed(i)
  fit.knn.original <- train(EventClass~., data=trainingDS, method="knn", metric=metric, trControl=control, na.action=na.omit)
  # cfm.knn.original = ReturnConfusionMatrix(myModel = fit.knn.original, myTesting = testingDS)
  # cfmName.knn.original = paste0("cfm_knn_original_",i)
  # myConfusionMatrixList[[cfmName.knn.original]] =  cfm.knn.original
  # 
  set.seed(i)
  fit.knn.under <- train(EventClass~., data=df_under, method="knn", metric=metric, trControl=control, na.action=na.omit)
  # cfm.knn.under = ReturnConfusionMatrix(myModel = fit.knn.under, myTesting = testingDS)
  # cfmName.knn.under = paste0("cfm_knn_under_",i)
  # myConfusionMatrixList[[cfmName.knn.under]] =  cfm.knn.under
  # 
  
  # set.seed(i)
  # fit.knn.over <- train(EventClass~., data=df_over, method="knn", metric=metric, trControl=control, na.action=na.omit)
  # cfm.knn.over = ReturnConfusionMatrix(myModel = fit.knn.over, myTesting = testingDS)
  # cfmName.knn.over = paste0("cfm_knn_over_",i)
  # myConfusionMatrixList[[cfmName.knn.over]] =  cfm.knn.over

  
  # set.seed(i)
  # fit.knn.rose <- train(EventClass~., data=df_rose, method="knn", metric=metric, trControl=control, na.action=na.omit)
  # cfm.knn.rose = ReturnConfusionMatrix(myModel = fit.knn.rose, myTesting = testingDS)
  # cfmName.knn.rose = paste0("cfm_knn_rose_",i)
  # myConfusionMatrixList[[cfmName.knn.rose]] =  cfm.knn.rose
  
  print(paste0("Done knn for well " , dfname))
  print(Sys.time())
  
  # CART
  set.seed(i)
  fit.rpart.original <- train(EventClass~., data=trainingDS, method="rpart", metric=metric, trControl=control, na.action=na.omit)
  # cfm.rpart.original = ReturnConfusionMatrix(myModel = fit.rpart.original, myTesting = testingDS)
  # cfmName.rpart.original = paste0("cfm_rpart_original_",i)
  # myConfusionMatrixList[[cfmName.rpart.original]] =  cfm.rpart.original
  # 
  
  set.seed(i)
  fit.rpart.under <- train(EventClass~., data=df_under, method="rpart", metric=metric, trControl=control, na.action=na.omit)
  # cfm.rpart.under = ReturnConfusionMatrix(myModel = fit.rpart.under, myTesting = testingDS)
  # cfmName.rpart.under = paste0("cfm_rpart_under_",i)
  # myConfusionMatrixList[[cfmName.rpart.under]] =  cfm.rpart.under
  # 
  set.seed(i)
  fit.rpart.over <- train(EventClass~., data=df_over, method="rpart", metric=metric, trControl=control, na.action=na.omit)
  # cfm.rpart.over = ReturnConfusionMatrix(myModel = fit.rpart.over, myTesting = testingDS)
  # cfmName.rpart.over = paste0("cfm_rpart_over_",i)
  # myConfusionMatrixList[[cfmName.rpart.over]] =  cfm.rpart.over
  # 
  print(paste0("Done decision trees for well " , dfname))
  print(Sys.time())
  # set.seed(i)
  # fit.rpart.rose <- train(EventClass~., data=df_rose, method="rpart", metric=metric, trControl=control, na.action=na.omit)
  # cfm.rpart.rose = ReturnConfusionMatrix(myModel = fit.rpart.rose, myTesting = testingDS)
  # cfmName.rpart.rose = paste0("cfm_rpart_rose_",i)
  # myConfusionMatrixList[[cfmName.rpart.rose]] =  cfm.rpart.rose
  
  #Naive Bayes
  set.seed(i)
  fit.nb.original <- train(EventClass~., data=trainingDS, method="nb", metric=metric, trControl=control, na.action=na.omit)
  # cfm.nb.original = ReturnConfusionMatrix(myModel = fit.nb.original, myTesting = testingDS)
  # cfmName.nb.original = paste0("cfm_nb_original_",i)
  # myConfusionMatrixList[[cfmName.nb.original]] =  cfm.nb.original

  set.seed(i)
  fit.nb.under <- train(EventClass~., data=df_under, method="nb", metric=metric, trControl=control, na.action=na.omit)
  # cfm.nb.under = ReturnConfusionMatrix(myModel = fit.nb.under, myTesting = testingDS)
  # cfmName.nb.under = paste0("cfm_nb_under_",i)
  # myConfusionMatrixList[[cfmName.nb.under]] =  cfm.nb.under

  set.seed(i)
  fit.nb.over <- train(EventClass~., data=df_over, method="nb", metric=metric, trControl=control, na.action=na.omit)
  # cfm.nb.over = ReturnConfusionMatrix(myModel = fit.nb.over, myTesting = testingDS)
  # cfmName.nb.over = paste0("cfm_nb_over_",i)
  # myConfusionMatrixList[[cfmName.nb.over]] =  cfm.nb.over
  # 
  print(paste0("Done naive base for well " , dfname))
  print(Sys.time())
  
  # 
  # set.seed(i)
  # fit.nb.rose <- train(EventClass~., data=df_rose, method="nb", metric=metric, trControl=control, na.action=na.omit)
  # cfm.nb.rose = ReturnConfusionMatrix(myModel = fit.nb.rose, myTesting = testingDS)
  # cfmName.nb.rose = paste0("cfm_nb_rose_",i)
  # myConfusionMatrixList[[cfmName.nb.rose]] =  cfm.nb.rose
  
  
  # SVM
  set.seed(i)
  fit.svm.original <- train(EventClass~., data=trainingDS, method="svmRadial", metric=metric, trControl=control, na.action=na.omit)
  # cfm.svm.original = ReturnConfusionMatrix(myModel = fit.svm.original, myTesting = testingDS)
  # cfmName.svm.original = paste0("cfm_svm_original_",i)
  # myConfusionMatrixList[[cfmName.svm.original]] =  cfm.svm.original
  # 
  set.seed(i)
  fit.svm.under <- train(EventClass~., data=df_under, method="svmRadial", metric=metric, trControl=control, na.action=na.omit)
  # cfm.svm.under = ReturnConfusionMatrix(myModel = fit.svm.under, myTesting = testingDS)
  # cfmName.svm.under = paste0("cfm_svm_under_",i)
  # myConfusionMatrixList[[cfmName.svm.under]] =  cfm.svm.under
  # 
  set.seed(i)
  fit.svm.over <- train(EventClass~., data=df_over, method="svmRadial", metric=metric, trControl=control, na.action=na.omit)
  # cfm.svm.over = ReturnConfusionMatrix(myModel = fit.svm.over, myTesting = testingDS)
  # cfmName.svm.over = paste0("cfm_svm_over_",i)
  # myConfusionMatrixList[[cfmName.svm.over]] =  cfm.svm.over
  # 
  print(paste0("Done svm for well " , dfname))
  print(Sys.time())
  
  # set.seed(i)
  # fit.svm.rose <- train(EventClass~., data=df_rose, method="svmRadial", metric=metric, trControl=control, na.action=na.omit)
  # cfm.svm.rose = ReturnConfusionMatrix(myModel = fit.svm.rose, myTesting = testingDS)
  # cfmName.svm.rose = paste0("cfm_svm_rose_",i)
  # myConfusionMatrixList[[cfmName.svm.rose]] =  cfm.svm.rose
  
  # Compare algorithms
  results <- resamples(list(glmOriginal=fit.glm.original,
                            glmUnder = fit.glm.under,
                            glmOver = fit.glm.over,
                            #glmRose = fit.glm.rose,
                            ldaOriginal=fit.lda.original,
                            ldaUnder = fit.lda.under,
                            ldaOver = fit.lda.over,
                            #ldaRose = fit.lda.rose,
                          # glmnetOriginal=fit.glmnet.original,
                           # glmnetUnder = fit.glmnet.under,
                            #glmnetOver = fit.glmnet.over,
                            #glmnetRose = fit.glmnet.rose,
                            knnOriginal=fit.knn.original,
                            knnUnder = fit.knn.under,
                            #knnRose = fit.knn.rose, 
                            rpartOriginal=fit.rpart.original,
                            rpartUnder= fit.rpart.under, 
                            rpartOver = fit.rpart.over,
                            #rpartRose = fit.rpart.rose,
                            svmOriginal=fit.svm.original,
                            svmUnder = fit.svm.under,
                            svmOver = fit.svm.over,
                            nbOriginal =  fit.nb.original,
                           nbUnder  = fit.nb.under,
                           nbOver = fit.nb.over
                            #svmRose = fit.svm.rose
                            ))

  
  myResults[[dfname]] = results
  
  listOfModels =list(glmOriginal=fit.glm.original,
                     glmUnder = fit.glm.under,
                     glmOver = fit.glm.over,
                     #glmRose = fit.glm.rose,
                     ldaOriginal=fit.lda.original,
                     ldaUnder = fit.lda.under,
                     ldaOver = fit.lda.over,
                     #ldaRose = fit.lda.rose,
                     #glmnetOriginal=fit.glmnet.original,
                     #glmnetUnder = fit.glmnet.under,
                     #glmnetOver = fit.glmnet.over,
                     #glmnetRose = fit.glmnet.rose,
                     knnOriginal=fit.knn.original,
                     knnUnder = fit.knn.under,
                     #knnRose = fit.knn.rose, 
                     rpartOriginal=fit.rpart.original,
                     rpartUnder= fit.rpart.under, 
                     rpartOver = fit.rpart.over,
                     #rpartRose = fit.rpart.rose,
                     svmOriginal=fit.svm.original,
                     svmUnder = fit.svm.under,
                     svmOver = fit.svm.over,
                     nbOriginal =  fit.nb.original,
                     nbUnder  = fit.nb.under,
                     nbOver = fit.nb.over
                     #svmRose = fit.svm.rose
                     )
  
  save(listOfModels, file = paste0(dfname,"_models_v2.rda"))
  
  ## boxplot to compare models
  scales = list(x = list(relation = "free"), y = list(relation = "free"))
  bwplot(results, scales = scales)
  
  ## dotplot to compare models
  ##scales = list(x = list(relation = "free"), y = list(relation = "free"))
  ##dotplot(results, scales = scales)
}
  ## check the difference between algorithms
  #diffs = diff(results)
  #summary(diffs)
  
  ## To tune an algorithm, say, SVM with its parameters (C and sigma) you can do the following
  ## grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 10, by=1))
  ## fit.svm <- train(Class~., data=dataset, method="svmRadial", metric=metric, tuneGrid=grid, preProc=c("BoxCox"), trControl=trainControl, na.action=na.omit)
  

  
  
  
  
  
  