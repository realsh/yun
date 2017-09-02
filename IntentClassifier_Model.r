# Intent classifier modeling(validation)

library(data.table)

library(tm)

library(caret)

library(rpart)


setwd("C://Users/Administrator/Desktop/IntentSearch")

Final_Train_data.dt <-data.table(read.csv("script.csv", header=TRUE))

total_set<-subset(Final_Train_data.dt[,3:5])

Final_Train_data.dt$idx <- 1:NROW(Final_Train_data.dt)

totlength <- nrow(Final_Train_data.dt)

Final_Train_data.dt[,round((.N/totlength)*100,digits = 1),by=.(intent)][order(-V1)]

myCorpus.cor <- Corpus(VectorSource(Final_Train_data.dt$transcript)) 

LiveChat.dtm.nostem <- DocumentTermMatrix(myCorpus.cor,
                                   
                                   control=list(weighting=weightTfIdf, minWordLength=2, minDocFreq=5,
                                                
                                                stemming=FALSE ,removeNumbers=TRUE, stopwords=TRUE))

LiveChat.dtm <- DocumentTermMatrix(myCorpus.cor,
                                   
                                   control=list(weighting=weightTfIdf, minWordLength=2, minDocFreq=5,
                                                
                                                stemming=TRUE ,removeNumbers=TRUE, stopwords=TRUE))


dim(LiveChat.dtm)

dim(LiveChat.dtm.nostem)

as.data.frame(LiveChat.dtm)

write.csv(as.data.frame(as.matrix(LiveChat.dtm,"stem_data.csv")))

write.csv(as.data.frame(as.matrix(LiveChat.dtm.nostem,"no_stem_data.csv")))

LiveChat.dtm <- DocumentTermMatrix(myCorpus.cor,
                                   
                                   control=list(minWordLength=2, minDocFreq=5,
                                                
                                                stemming=TRUE ,removeNumbers=TRUE, stopwords=TRUE))

dim(LiveChat.dtm)

colnames(LiveChat.dtm)


rowTotals <- apply(LiveChat.dtm , 1, sum)

colTotals <- apply(LiveChat.dtm , 2, sum)



table(rowTotals)

table(colTotals)


# > dim(LiveChat.dtm)

# [1] 1364  826

freqterms100 <- findFreqTerms(LiveChat.dtm, 100)

length(freqterms100) # 23

freqterms90 <- findFreqTerms(LiveChat.dtm, 90)

length(freqterms90) # 33

freqterms80 <- findFreqTerms(LiveChat.dtm, 80)

length(freqterms80) # 37

freqterms70 <- findFreqTerms(LiveChat.dtm, 70)

length(freqterms70) # 41

freqterms60 <- findFreqTerms(LiveChat.dtm, 60)

length(freqterms60) # 49

freqterms50 <- findFreqTerms(LiveChat.dtm, 50)

length(freqterms50) # 58

freqterms40 <- findFreqTerms(LiveChat.dtm, 40)

length(freqterms40) # 73

freqterms30 <- findFreqTerms(LiveChat.dtm, 30)

length(freqterms30) # 90

freqterms20 <- findFreqTerms(LiveChat.dtm, 20)

length(freqterms20) # 126

freqterms10 <- findFreqTerms(LiveChat.dtm, 10)

length(freqterms10) # 188






LiveChat.dtm.df <- as.data.frame(inspect(LiveChat.dtm))

LiveChat.dtm.m.dt <- as.data.table(as.matrix(LiveChat.dtm))

LiveChat.dtm.m.dt$idx <- 1:NROW(LiveChat.dtm.m.dt)

LiveChat.dtm.m.intent.dt <- merge(LiveChat.dtm.m.dt,Final_Train_data.dt,by = c("idx"))

dim(LiveChat.dtm.m.intent.dt) 

View(LiveChat.dtm.m.intent.dt)

names(LiveChat.dtm.m.intent.dt)

LiveChat.dtm.m.intent.dt <- LiveChat.dtm.m.intent.dt[,c(1,828,829,2:827)]

View(LiveChat.dtm.m.intent.dt)

LiveChat.dtm.m.intent.dt$intent<-total_set$intent

LiveChat.dtm.m.intent.dt$intent.1<-total_set$intent.1

LiveChat.dtm.m.intent.dt$intent

write.csv(LiveChat.dtm.m.intent.dt,"dtm2.csv")

# LiveChat.dtm.m.intent.dt: index/dtm/intent




k <- 10

folds <- createFolds(total_set$intent, k) # validation setting

str(folds$Fold01)




### fold 1 validation 

for (j in 1:k) {

    train_idx <- folds$Fold01
  
    data.train <- LiveChat.dtm.m.intent.dt[-train_idx, ]
  
    data.validation <- LiveChat.dtm.m.intent.dt[train_idx, ]
}




# 1st - freq 100(23)
fitTree_100 <- rpart(intent ~  agent+appreci+assist+can+chat+day+door+electron+great+   
                 
                       live+may+need+num+number+order+part+pleas+survey+thank+time+today+visitor+zone, data.train)

newdata<-as.data.frame(data.validation)

data.validation$pred<-predict(fitTree_100, newdata, type="class")


conf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall 

# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 

# 6.740741e-01   6.439062e-01   5.881207e-01   7.521983e-01   1.111111e-01   6.519082e-54 

# 2nd - freq 90(33)

fitTree_90 <- rpart(intent ~ agent + anyth + appreci + assist + busi + can + chat + contact + day
                 
                    + door + electron +els + great + help + live + may + model + need + num
                 
                    + number + one + order + part + pleas + question + right + survey + thank + time
                 
                    + today + visitor + will + zone, data.train)

newdata<-as.data.frame(data.validation)

data.validation$pred<-predict(fitTree_90, newdata, type="class")


conf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall 

# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 

# 6.888889e-01   6.598680e-01   6.035545e-01   7.657220e-01   1.111111e-01   2.243113e-56 

# 3rd - freq 70(41)

fitTree_70 <- rpart(intent ~  agent+anyth+appreci+assist+bin+busi+call+can+chat+concern+contact+
     
                      day+door+electron+els+great+help+hesit+know+live+may+model+need+num+number+one+
                    
                      open+order+part+pleas+question+refriger+right+survey+thank+time+today+visitor+
                    
                      welcom+will+zone, data.train )

newdata <- as.data.frame(data.validation)

data.validation$pred<-predict(fitTree_70, newdata, type="class")


conf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall # accuracy: 0.667

# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 

# 6.666667e-01   6.359660e-01   5.804368e-01   7.454029e-01   1.111111e-01   1.056917e-52 

# 4th - freq 60(49)

fitTree_60 <- rpart(intent ~  agent+anyth+appreci+assist+bin+busi+call+can+central+chat+concern+contact+

                      day+door+electron+els+encompass+end+great+help+hesit+know+left+live+may+mcm+model+much+need+num+number+one+
                      
                      open+order+part+pleas+provid+question+refriger+replac+right+survey+thank+time+today+visitor+
                      
                      welcom+will+zone, data.train )

newdata<-as.data.frame(data.validation)

data.validation$pred<-predict(fitTree_60, newdata, type="class")

conf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall # accuracy: 0.6741

# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 

# 6.740741e-01   6.422764e-01   5.881207e-01   7.521983e-01   1.111111e-01   6.519082e-54




# 5th - freq 50(58)

fitTree_50 <- rpart(intent ~  agent+anyth+appreci+assist+bin+bottom+busi+button+call+can+central+chat+choos+concern+  
                    
                      contact+day+distributor+door+eastern+electron+els+encompass+end+good+great+help+hesit+just+know+
                    
                      left+live+look+may+mcm+model+much+need+num+number+one+open+order+part+pleas+provid+question+refriger+
                    
                      replac+right+shelf+survey+thank+time+today+visitor+welcom+will+zone , data.train )

newdata <- as.data.frame(data.validation)

data.validation$pred <- predict(fitTree_50, newdata, type="class")

conf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall # accuracy: 0.7037

# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 

# 7.037037e-01   6.749338e-01   6.190789e-01   7.791535e-01   1.111111e-01   6.731765e-59 



# 6th - freq 40(73)

fitTree_40 <- rpart(intent ~   agent+anyth+appreci+assist+bin+bottom+busi+button+call+can+central+chat+choos+concern+contact
     
                    +day+distributor+door+drawer+eastern+electron+els+encompass+end+est+experi+filter+fridg+good+great+help
                    
                    +hesit+just+know+left+like+live+locat+look+may+mcm+model+much+need+now+num+number+one+open+order+part
                    
                    +particip+phone+pleas+pleasur+provid+question+refriger+replac+right+shelf+side+survey+thank+time+today
                    
                    +unit+via+visitor+websit+welcom+will+zone, data.train )

newdata <- as.data.frame(data.validation)

data.validation$pred <- predict(fitTree_40, newdata, type="class")


conf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall # accuracy: 0.7037

# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 

# 7.037037e-01   6.749338e-01   6.190789e-01   7.791535e-01   1.111111e-01   6.731765e-59            NaN 


# 7th - freq 30(90)

fitTree_30 <- rpart(intent ~   agent+anyth+appreci+assembl+assist+avail+bin+bottom+busi+button+call+can+central+chat+choos+com+concern+contact
                    
                    +day+distributor+door+drawer+eastern+electron+els+encompass+end+est+experi+filter+find+freezer+fridg+get+good+great+hello+help
                    
                    +hesit+high+ice+just+know+left+like+live+locat+look+may+mcm+model+much+need+now+num+number+one+open+order+pacif+part
                    
                    +particip+phone+pleas+pleasur+press+price+provid+purchas+question+refriger+replac+right+session+shelf+side+survey+thank+time+today
                    
                    +top+unit+via+visitor+water+websit+welcom+will+yes+zone, data.train )

newdata <- as.data.frame(data.validation)

data.validation$pred <- predict(fitTree_30, newdata, type="class")

conf_dt<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall # accuracy: 0.7037

# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 

# 7.185185e-01   6.915213e-01   6.346975e-01   7.924889e-01   1.111111e-01   1.755429e-61            NaN 

write.csv(conf_dt$table, "dt_conf.csv")

printcp(fitTree_30) # display the results

# Variables actually used in tree construction:

#  [1] agent    anyth    central  day      eastern  good     look     need     num      purchas  question

#  [12] replac   thank    zone  

plotcp(fitTree_30) # visualize cross-validation results

summary(fitTree_30)




# plot tree 

plot(fitTree_30, uniform=TRUE, 

          main="Classification Tree for intent")

text(fitTree_30, use.n=TRUE, all=TRUE, cex=.65)


# 8th - freq 20(126)


fitTree_20 <- rpart(intent ~   aap+agent+allow+andrew+anyth+appreci+area+assembl+assist+avail+basket+bin+bottom+broken+busi+button+call+can+central+chat+check+choos+click+com+concern+contact
                    
                    +day+distributor+door+drawer+eastern+electron+els+encompass+end+est+exact+experi+feedback+fill+filter+find+freezer+fridg+gasket+get+give+glass+good+great+handl+hear+hello+help
                    
                    +hesit+high+hope+ice+improv+insid+just+know+left+like+live+locat+look+love+may+mcm+model+much+need+new+nice+now+num+number+okay+one+open+opinion+order+pacif+part
                    
                    +particip+patienc+phone+pleas+pleasur+press+price+provid+purchas+question+recent+refriger+regard+replac+right+see+session+shelf+shelv+short+side+sorri+start+sure+survey+thank+time+today
                    
                    +top+unit+url+valuabl+via+visitor+water+websit+welcom+will+www+yes+zone, data.train )

newdata <- as.data.frame(data.validation)

data.validation$pred<-predict(fitTree_20, newdata, type="class")


conf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall # accuracy: 0.7185

# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 

# 7.185185e-01   6.915213e-01   6.346975e-01   7.924889e-01   1.111111e-01   1.755429e-61            NaN 

write.csv(conf$table, "checking.csv")



# 9th - freq 10(188)

fitTree_10 <- rpart(intent ~   aap+afternoon+agent+ahead+allow+also+andrew+anyth+appreci+area+ask+assembl+assist+author+avail+base+basket+belong+better+bin+bottom+broke+broken+busi+button+buy+bye+call+can+central+chat+check+choos+click+com+concern+contact
                    
                    +correct+cover+day+dealer+discuss+distributor+door+drawer+eastern+electron+els+encompass+end+est+exact+experi+feedback+fill+filter+find+freezer+fridg+front+futur+gallon+gasket+get+give+glass+good+great+handl+happen+happi+hear+hello+help
                    
                    +hesit+high+hold+hope+hour+ice+improv+inquiri+insid+invit+just+kind+know+left+let+like+live+locat+look+love+make+maker+may+mcm+mcmelectron+middl+model+moment+morn+much+need+new+nice+now+num+number+okay+one+open+oper+opinion+order+pacif+part
                    
                    +particip+patienc+phone+piec+place+plastic+pleas+pleasur+press+price+provid+prior+problem+provid+pst+purchas+question+rate+reason+recent+refriger+regard+replac+right+sat+second+secur+see+serv+session+shelf+shelv+short+side+sorri+spring+start+storag+support+sure+survey+tell+thank+think+time+today
                    
                    +top+tray+tri+two+unabl+unit+url+use+valuabl+via+visit+visitor+want+water+websit+welcom+well+whatev+will+wonder+www+year+yes+zone, data.train )

newdata <- as.data.frame(data.validation)

data.validation$pred<-predict(fitTree_10, newdata, type="class")

conf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall # accuracy: 0.7185

# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 

# 7.185185e-01   6.915213e-01   6.346975e-01   7.924889e-01   1.111111e-01   1.755429e-61            NaN 




# key word(term) (14 word)/ decision Tree - 0.7185


fitTree_imp<-rpart(intent ~ agent + anyth + central + day + eastern + good + look + need + num + purchas + question + replac + thank + zone , data.train)

newdata<-as.data.frame(data.validation)

data.validation$pred<-predict(fitTree_imp, newdata, type="class")

conf$byClass

conf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall # accuracy: 0.7185

#  [1] agent    anyth    central  day      eastern  good     look     need     num      purchas  question

#  [12] replac   thank    zone 

printcp(fitTree_imp) # display the results

# Variables actually used in tree construction:

#  [1] agent    anyth    central  day      eastern  good     look     need     num      purchas  question

#  [12] replac   thank    zone  

plotcp(fitTree_imp) # visualize cross-validation results



# create attractive postscript plot of tree 

post(fitTree_imp, file = "tree.jpg", 

          title = "Classification Tree for intent")


conf$byClass

# prune the tree 

pfitTree_imp<- prune(fitTree_imp, cp=   fitTree_imp$cptable[which.min(fitTree_imp$cptable[,"xerror"]),"CP"])

# plot the pruned tree 

plot(pfitTree_imp, uniform=TRUE, 

          main="Pruned Classification Tree for intent")

text(pfitTree_imp, use.n=TRUE, all=TRUE, cex=.65)

post(pfitTree_imp, file = "ptree.jpg", 

          title = "Pruned Classification Tree for intent")



# SVM model trying - 0.4963

library(e1071)

svm_imp<-svm(intent ~ agent + anyth + central + day + eastern + good + look + need + num + purchas + question + replac + thank + zone , data.train)

newdata<-as.data.frame(data.validation)

summary(svm_imp)

data.validation$pred<-predict(svm_imp, newdata)

conf_svm<-confusionMatrix(data.validation$pred,data.validation$intent)

write.csv(conf_svm$table,"svm_conf.csv")



# random forest model trying - 0.7111

library(randomForest)

rf_imp<-randomForest(intent ~ agent + anyth + central + day + eastern + good + look + need + num + purchas + question + replac + thank + zone , data.train)

print(rf_imp)


data.validation$pred<-predict(rf_imp, data.validation)

conf_rf<-confusionMatrix(data.validation$pred,data.validation$intent)

conf$overall

conf$byClass

write.csv(conf_rf$table,"RF_conf.csv")


# knn model trying 

library(class)


#  for(i in 1:10){

#    #Apply knn with k = i

#    predict<-knn(data.train[,4:829],data.validation[,4:829],

#                 data.train$intent,k=i)

#    intent_acc<-c(intent_acc,

#                mean(predict==data.validation$intent))

#    print(i)

#    print(predict)

#  }

predict<-knn(data.train[,4:829],data.validation[,4:829],

                          data.train$intent,k=3)

data.validation$pred1.knn<-predict


predict<-knn(data.train[,4:829],data.validation[,4:829],

                          data.train$intent,k=5)

data.validation$pred2.knn<-predict

predict<-knn(data.train[,4:829],data.validation[,4:829],

                          data.train$intent,k=7)

data.validation$pred3.knn<-predict

predict<-knn(data.train[,4:829],data.validation[,4:829],

                          data.train$intent,k=9)

data.validation$pred4.knn<-predict

predict<-knn(data.train[,4:829],data.validation[,4:829],

                          data.train$intent,k=15)

data.validation$pred5.knn<-predict

conf1<-confusionMatrix(data.validation$pred1.knn,data.validation$intent)

write.csv(conf1$table,"knn_conf.csv") # 0.6811

conf2<-confusionMatrix(data.validation$pred2.knn,data.validation$intent)

conf2$overall # 0.6517

conf3<-confusionMatrix(data.validation$pred3.knn,data.validation$intent)

conf3$overall # 0.6377

conf4<-confusionMatrix(data.validation$pred4.knn,data.validation$intent)

conf4$overall # 0.6232

conf5<-confusionMatrix(data.validation$pred5.knn,data.validation$intent)

conf5$overall # 0.5869


## neural net 

install.packages("neuralnet")

library(neuralnet)

data.train$intent<-as.numeric(data.train$intent)

nn <- neuralnet(intent ~ agent + anyth + central + day + eastern + good + look + need + num + purchas + question + replac + thank + zone, data=data.train,  hidden=10, threshold=0.01)

print(nn)

nn$net.result

plot(nn)

data.validation$intent<-as.numeric(data.validation$intent)

data.validation<-as.data.frame(data.validation)

nn.results<-compute(nn, (1:10)^2)$net.result

pr.nn<-compute(nn, data.validation)

# data.validation$pred<-predict(, newdata)

a<-prediction(nn)

nn$net.result





## GBM

install.packages("gbm")

library(gbm)


gbm1 <-

    gbm(intent ~ agent + anyth + central + day + eastern + good + look + need + num + purchas + question + replac + thank + zone,         # formula
  
            data=data.train,                   # dataset
      
        var.monotone=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), # -1: monotone decrease,
      
        # +1: monotone increase,
      
        #  0: no monotone restrictions
      
        distribution="gaussian",     # see the help for other choices
      
        n.trees=1000,                # number of trees
      
        shrinkage=0.05,              # shrinkage or learning rate,
      
        # 0.001 to 0.1 usually work
      
        interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
      
        bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
      
        train.fraction = 0.5,        # fraction of data for training,
      
        # first train.fraction*N used for training
      
        n.minobsinnode = 10,       # minimum total weight needed in each node
      
        # cv.folds = 3,                # do 3-fold cross-validation
      
        keep.data=TRUE,              # keep a copy of the dataset with the object
      
        verbose=FALSE,               # don't print out progress
      
        n.cores=1)                   # use only a single core (detecting #cores is

gbm1 <-

    gbm(intent ~ agent + anyth + central + day + eastern + good + look + need + num + purchas + question + replac + thank + zone,         # formula
  
            data=data.train)


best.iter <- gbm.perf(gbm1,method="OOB")

print(best.iter)

best.iter <- gbm.perf(gbm1,method="test")

print(best.iter)


best.iter <- gbm.perf(gbm1,method="cv")

print(best.iter)

summary(gbm1,n.trees=1000)  



# compactly print the first and last trees for curiosity

print(pretty.gbm.tree(gbm1,1))

print(pretty.gbm.tree(gbm1,gbm1$n.trees))

f.predict <- predict(gbm1,data.validation,1000)

print(sum((data.validation$intent-f.predict)^2))


## caret package

fitControl <- trainControl(## 10-fold CV

    method = "repeatedcv",

      number = 10,

      ## repeated ten times
  
    repeats = 10)



set.seed(825)

gbmFit1 <- train(Class ~ ., data = training, 

                                  method = "gbm", 
                 
                 trControl = fitControl,
                 
                 ## This last option is actually one
                 
                 ## for gbm() that passes through
                 
                 verbose = FALSE)

gbmFit1

## Stochastic Gradient Boosting 

## 
## 157 samples
##  60 predictor
##   2 classes: 'M', 'R' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 142, 142, 140, 142, 142, 141, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   1                   50      0.7609191  0.5163703
##   1                  100      0.7934216  0.5817734
##   1                  150      0.7977230  0.5897796
##   2                   50      0.7858235  0.5667749
##   2                  100      0.8188897  0.6316548
##   2                  150      0.8194363  0.6329037
##   3                   50      0.7895686  0.5726290
##   3                  100      0.8130564  0.6195719
##   3                  150      0.8221348  0.6383441
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## 
## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
## Accuracy was used to select the optimal model using  the largest value.
## The final values used for the model were n.trees = 150,
##  interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.





## Xgboost 

install.packages("xgboost")

library(xgboost)

df_train = as.data.frame(data.train)

df_test = as.data.frame(data.validation)

labels = df_train['intent']

df_train<- df_train[,1:829]

df_train[,830]<-as.numeric(df_train[,830])

df_train<-as.matrix(df_train[,1:830])


xgb <- xgboost(data = df_train[,1:829], 

               label = df_train[,830],
               
               eta = 0.1,
               
               max_depth = 15,
               
               nround=25,
               
               objective = "multi:softprob",
               
               num_class = 17
               
               )


# predict values in test set

xgb_pred<-predict(xgb, data.matrix(df_test[,1:829]))

df_test$xgb_pred<-xgb_pred

df_test[,830]<-as.numeric(df_test[,830])

conf<-confusionMatrix(df_test$xgb_pred,df_test$intent)

conf$overall

df_test$intent
#  , 
 #                eta = 0.1,
  #               max_depth = 15, 
   #              nround=25, 
    #             subsample = 0.5,
     #            colsample_bytree = 0.5,
      #           seed = 1,
       #          eval_metric = "merror",
        #         objective = "multi:softprob",
         #        num_class = 12,
          #       nthread = 3
  #
