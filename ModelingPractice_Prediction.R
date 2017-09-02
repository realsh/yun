## load data

setwd("C://")

data<-read.table("vertebral_column_2C.dat")

head(data)

data[2,7]

## pre-processing

# input variables

pelvic_incidence<-data[1:310,1]

pelvic_tilt<-data[1:310,2]

lumbar_lordosis_angle<-data[1:310,3]

sacral_slope<-data[1:310,4]

pelvic_radius<-data[1:310,5]

grade_of_spondylolisthesis<-data[1:310,6]


# outputº¯¼ö binary·Î

i<-c(1:310)

y<-ifelse(data[i,7]=="AB",1,0)

data<-data.frame(pelvic_incidence,pelvic_tilt,lumbar_lordosis_angle,sacral_slope,

                 pelvic_radius,grade_of_spondylolisthesis,y)

attach(data)

print(data)

## 3-fold cross validation 

random_300=sample(1:nrow(data),300,replace=F)

sample_data<-data[random_300,]

head(sample_data)

group1<-sample_data[1:100,]

group2<-sample_data[101:200,]

group3<-sample_data[201:300,]

group1[,7]

head(group1)

print(group1)


#############################################################################

###1. Logistic Regression

# Kfold1

train1<-rbind(group2,group3)

Kfold1_results<-glm( y ~ pelvic_incidence + pelvic_tilt + lumbar_lordosis_angle + sacral_slope +

                       pelvic_radius + grade_of_spondylolisthesis, family="binomial", data=train1)

summary(Kfold1_results)

rep(1, nobs)

library(car)

install.packages("stats")

glm.fit = predict(Kfold1_results, weights =.1,type="response",weight=0.1, newdata=group1)

glm.fit

glm.pred1 = rep(0,100)

glm.pred1[glm.fit>.5]=1

confusion=table(glm.pred1, group1[,7])

table(glm.pred1)

accuracy=(confusion[1,1]+confusion[2,2])/100

precisionNO=(confusion[1,1])/(confusion[1,1]+confusion[1,2])

precisionAB=(confusion[2,2])/(confusion[2,1]+confusion[2,2])

recallNO=(confusion[1,1])/(confusion[1,1]+confusion[2,1])

recallAB=(confusion[2,2])/(confusion[1,2]+confusion[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

fscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)



# Kfold2

train2<-rbind(group1,group3)

Kfold2_results<-glm( y ~ pelvic_incidence + pelvic_tilt + lumbar_lordosis_angle + sacral_slope +

                       pelvic_radius + grade_of_spondylolisthesis, family="binomial", data=train2)

summary(Kfold2_results)

glm.fit2 = predict(Kfold2_results, type="response", newdata=group2)

glm.fit2

glm.pred2= rep(0,100)

glm.pred2[glm.fit2>.5]=1

confusion=table(glm.pred2, group2[,7])

table(glm.pred2)

accuracy=(confusion[1,1]+confusion[2,2])/100

precisionNO=(confusion[1,1])/(confusion[1,1]+confusion[1,2])

precisionAB=(confusion[2,2])/(confusion[2,1]+confusion[2,2])

recallNO=(confusion[1,1])/(confusion[1,1]+confusion[2,1])

recallAB=(confusion[2,2])/(confusion[1,2]+confusion[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

fscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)



# Kfold3

train3<-rbind(group1,group2)

Kfold3_results<-glm( y ~ pelvic_incidence + pelvic_tilt + lumbar_lordosis_angle + sacral_slope +

                       pelvic_radius + grade_of_spondylolisthesis, family="binomial", data=train3)

summary(Kfold3_results)

glm.fit3 = predict(Kfold3_results, type="response", newdata=group3)

glm.fit3

glm.pred3= rep(0,100)

glm.pred3[glm.fit3>.5]=1

confusion=table(glm.pred3, group3[,7])

table(glm.pred3)

accuracy=(confusion[1,1]+confusion[2,2])/100

precisionNO=(confusion[1,1])/(confusion[1,1]+confusion[1,2])

precisionAB=(confusion[2,2])/(confusion[2,1]+confusion[2,2])

recallNO=(confusion[1,1])/(confusion[1,1]+confusion[2,1])

recallAB=(confusion[2,2])/(confusion[1,2]+confusion[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

fscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)



# training and calculate odds ratio 

results<-glm( y ~ pelvic_incidence + pelvic_tilt + lumbar_lordosis_angle + sacral_slope +

                pelvic_radius + grade_of_spondylolisthesis, family="binomial", data=data)

summary(results)

glm.fit = predict(results, type="response", newdata=data)

glm.fit

glm.pred= rep(0,310)

glm.pred[glm.fit>.5]=1

confusion=table(glm.pred, data[,7])

table(glm.pred2)

exp(coef(results))

exp(confint(Kfold1_results))

require(moonBook)

extractOR(Kfold1_results)


## evaluation

results <- glm(y ~ pelvic_incidence + pelvic_tilt + lumbar_lordosis_angle + sacral_slope +

                 pelvic_radius + grade_of_spondylolisthesis, family="binomial")

summary(results)

head(results$fitted.values)



#############################################################################

###2. Decision Tree

## decision tree 3-fold - kfold1

library(rpart)

library(rpart.plot)

install.packages("rpart.plot")



train1<-rbind(group2,group3)

model1<- rpart(y ~ pelvic_incidence + pelvic_tilt + lumbar_lordosis_angle + sacral_slope +

                 pelvic_radius + grade_of_spondylolisthesis, method="class", data=train1, control=rpart.control(maxdepth=5))

plot(model1, compress=T, uniform=TRUE)

text(model1, use.n=TRUE, all=TRUE, cex=0.7)

# test set

pred_3Tree1<- predict(model1, newdata=group1, type="class")

plot(pred_3Tree1, compress=T, uniform=TRUE)

# confusion matrix

real=group1[,7]

confusion1=table(pred_3Tree1,real)

## printing output
accuracy=(confusion1[1,1]+confusion1[2,2])/100

precisionNO=(confusion1[1,1])/(confusion1[1,1]+confusion1[1,2])

precisionAB=(confusion1[2,2])/(confusion1[2,1]+confusion1[2,2])

recallNO=(confusion1[1,1])/(confusion1[1,1]+confusion1[2,1])

recallAB=(confusion1[2,2])/(confusion1[1,2]+confusion1[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

fscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)


## decision tree 3-fold- kfold2

train2<-rbind(group1,group3)

model2<- rpart(y ~ pelvic_incidence + pelvic_tilt + lumbar_lordosis_angle + sacral_slope +

                 pelvic_radius + grade_of_spondylolisthesis, method="class", data=train2, control=rpart.control(maxdepth=5))

plot(model2, compress=T, uniform=TRUE)

text(model2, use.n=TRUE, all=TRUE, cex=0.7)

pred2<- predict(model2, newdata=group2, type="class")

plot(pred2, compress=T, uniform=TRUE)

real=group2[,7]

confusion2=table(pred2,real)

table(pred2, group2[,7])

table(pred2)


accuracy=(confusion2[1,1]+confusion2[2,2])/100

precisionNO=(confusion2[1,1])/(confusion2[1,1]+confusion2[1,2])

precisionAB=(confusion2[2,2])/(confusion2[2,1]+confusion2[2,2])

recallNO=(confusion2[1,1])/(confusion2[1,1]+confusion2[2,1])

recallAB=(confusion2[2,2])/(confusion2[1,2]+confusion2[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

fscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)


## decision tree 3-fold- kfold3

train3<-rbind(group1,group2)

model3<- rpart(y ~ ., method="class", data=train3, control=rpart.control(maxdepth=5))

plot(model3, compress=T, uniform=TRUE)

text(model3, use.n=TRUE, all=TRUE, cex=0.7)

pred3<- predict(model3, newdata=group3, type="class")

plot(pred3, compress=T, uniform=TRUE)

real=group3[,7]

confusion3=table(pred3,real)

table(pred3, group3[,7])

table(pred3)


accuracy=(confusion3[1,1]+confusion3[2,2])/100

precisionNO=(confusion3[1,1])/(confusion3[1,1]+confusion3[1,2])

precisionAB=(confusion3[2,2])/(confusion3[2,1]+confusion3[2,2])

recallNO=(confusion3[1,1])/(confusion3[1,1]+confusion3[2,1])

recallAB=(confusion3[2,2])/(confusion3[1,2]+confusion3[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

fscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)


## decision tree - training total data

model4<-rpart(y~., method="class", data=data, control=rpart.control(maxdepth=4))

plot(model4, compress=T, uniform=TRUE)

text(model4, use.n=TRUE, all=TRUE, cex=0.7)


#############################################################################

###3. K-nn


# normalization

normalize <- function(x) {

  return ((x - mean(x)) / sd(x) ) }

data_n <- as.data.frame(lapply(data[1:6], normalize))

install.packages("class")

library(class)



# kfold1

train1<-rbind(group2,group3)

head(train1)

head(group1)

head(data)

knn_pred<-knn(train=train1, test=group1, cl=train1$y ,k=7)

knn_pred


real=group1[,7]

confusion=table(knn_pred,real)

table(knn_pred)

head(data_n)

accuracy=(confusion[1,1]+confusion[2,2])/100

precisionNO=(confusion[1,1])/(confusion[1,1]+confusion[1,2])

precisionAB=(confusion[2,2])/(confusion[2,1]+confusion[2,2])

recallNO=(confusion[1,1])/(confusion[1,1]+confusion[2,1])

recallAB=(confusion[2,2])/(confusion[1,2]+confusion[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

fscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)



# kfold2

train2<-rbind(group1,group3)

knn_pred<-knn(train=train2, test=group2, cl=train2$y ,k=7)


real=group2[,7]

confusion=table(knn_pred,real)

table(knn_pred)

head(data_n)

accuracy=(confusion[1,1]+confusion[2,2])/100

precisionNO=(confusion[1,1])/(confusion[1,1]+confusion[1,2])

precisionAB=(confusion[2,2])/(confusion[2,1]+confusion[2,2])

recallNO=(confusion[1,1])/(confusion[1,1]+confusion[2,1])

recallAB=(confusion[2,2])/(confusion[1,2]+confusion[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

fscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)




# kfold 3 

train3<-rbind(group1,group2)

knn_pred<-knn(train=train3, test=group3, cl=train3$y ,k=7)


real=group3[,7]

confusion=table(knn_pred,real)

table(knn_pred)

head(data_n)

accuracy=(confusion[1,1]+confusion[2,2])/100

precisionNO=(confusion[1,1])/(confusion[1,1]+confusion[1,2])

precisionAB=(confusion[2,2])/(confusion[2,1]+confusion[2,2])

recallNO=(confusion[1,1])/(confusion[1,1]+confusion[2,1])

recallAB=(confusion[2,2])/(confusion[1,2]+confusion[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

rfscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)





# training total data

train3<-rbind(group1,group2)

knn_pred<-knn(train=data, test=data, cl=data$y ,k=7)

real=data[,7]

confusion=table(knn_pred,real)

table(knn_pred)

head(data_n)

accuracy=(confusion[1,1]+confusion[2,2])/310

precisionNO=(confusion[1,1])/(confusion[1,1]+confusion[1,2])

precisionAB=(confusion[2,2])/(confusion[2,1]+confusion[2,2])

recallNO=(confusion[1,1])/(confusion[1,1]+confusion[2,1])

recallAB=(confusion[2,2])/(confusion[1,2]+confusion[2,2])

fscore_NO=(2*precisionNO*recallNO/(precisionNO+recallNO))

rfscore_AB=(2*precisionAB*recallAB/(precisionAB+recallAB))

c(accuracy,precisionNO,precisionAB,recallNO,recallAB,fscore_NO,fscore_AB)



















