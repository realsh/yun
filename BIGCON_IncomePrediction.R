# 2016 BIGCONTEST 

# load data

setwd("C://")

CLAIM<-read.csv("Edited_CLAIM_DATA.csv", header=T, sep=",")

CUST<-read.csv("Edited_CUST_DATA.csv", header=T, sep=",")

CNTT<-read.csv("Edited_CNTT_DATA.csv", header=T, sep=",")

str(CLAIM)

str(CUST)

str(CNTT)


###########################################################################

install.packages("data.table")

library(data.table)

install.packages("e1071")

library("e1071")



# table join -> Merge CLAIM, CUST, CNTT TABLE

#1) CLAIM, CNTT table join - key=POLY_NO

C_Table<- merge(CNTT, CLAIM, by = "POLY_NO")

# remove overlap variables

c1<-C_Table$CUST_ID.x

c2<-C_Table$CUST_ID.y

c<-c1-c2

C_Table$중복<-c

write.csv(C_Table,"C_Table.csv")

New_C<-subset(C_Table, C_Table$중복==0)

str(New_C)

head(New_C)

colnames(New_C)

New_C<-New_C[,-61] # 중복행 제거한 후 데이터갱신

New_C<-New_C[,-22] # CUST_ID.y 제거한 후 데이터갱신

CUST_ID<-New_C[,2]

New_C$CUST_ID<-CUST_ID

New_C<-New_C[,-2]

write.csv(New_C,"New_C.csv") ## var 59


#2) New_C(CLAIM, CNTT), CUST table join - key=CUST_NO

C_Table2<-merge(New_C, CUST, by="CUST_ID")

str(C_Table2) # var 85

C_Table2$CNTT_RECP_SQNO<-C_Table2$CNTT_RECP_SQNO*0.0000001

C_Table2$CNTT_RECP_SQNO

write.csv(C_Table2,"JOINED.csv") ## CUST/CLAIM/CNTT merged table

str(CUST) # var 27

setwd("C://output")

C_Table2<-read.csv("JOINED.csv")



## add variable - 청약서소득-추정소득(변수명: INCM_VALUE), 입원/통원기간(변수명: HOSP_OTPA_PERIOD)

INCM_VALUE <- C_Table2$MNTH_INCM_AMT - C_Table2$CUST_INCM

C_Table2$INCM_VALUE <- INCM_VALUE

HOSP_OTPA_PERIOD <- C_Table2$HOSP_OTPA_ENDT - C_Table2$OTPA_STDT

C_Table2$HOSP_OTPA_PERIOD <- HOSP_OTPA_PERIOD

head(C_Table2$HOSP_OTPA_ENDT)


# divide train set / test set

train<-subset(C_Table2, C_Table2$DIVIDED_SET==1)

test<-subset(C_Table2, C_Table2$DIVIDED_SET==2)

write.csv(train,"BGCON_TRAIN.csv")

write.csv(test,"BGCON_TEST.csv")




train_n<-cbind(train[,62], train[,63], train[,66], train[,71], train[,72], 

                 train[,73], train[,78], train[,79], train[,80], train[,81], train[,82], 

                 train[,83], train[,84], train[,27], train[,28], train[,29], train[,37],

                 train[,38], train[,41], train[,42], train[,47], train[,48], train[,49],

                 train[,51], train[,52], train[,53], train[,54], train[,55], train[,56],

                 train[,57], train[,7], train[,9], train[,12], train[,13], train[,14],

                 train[,16], train[,17], train[,18], train[,19], train[,20], train[,21])

train_n<-train_n[,-24]

head(train_n)

str(train_n)



train<-subset(TRAIN[1:10000,], select=c(CUST_ID, SIU_CUST_YN, AGE, RESI_COST, CUST_RGST, TOTALPREM, CHLD_CNT, LTBN_CHLD_AGE, 

                                          MAX_PRM, CUST_INCM, RCBASE_HSHD_INCM, JPBASE_HSHD_INCM, POLY_NO, RECP_DATE, ORIG_RESN_DATE, RESN_DATE, HOSP_OTPA_STDT, HOSP_OTPA_ENDT,

                                          VLID_HOSP_OTDA, HOUSE_HOSP_DIST, DMND_AMT, PAYM_AMT, SELF_CHAM, NON_PAY, TAMT_SFCA,

                                          PATT_CHRG_TOTA, DSCT_AMT, COUNT_TRMT_ITEM, NON_PAY_RATIO, CNTT_YM, REAL_PAYM_TERM,

                                          EXPR_YM, EXTN_YM, LAPS_YM, MAIN_INSR_AMT, SUM_ORIG_PREM, MNTH_INCM_AMT, DISTANCE))
str(train)


# making model - svm 

svm_model<-svm(train$SIU_CUST_YN~., data=train)

test1<-subset(TRAIN[10001:50000,], select=c(CUST_ID, SIU_CUST_YN, AGE, RESI_COST, CUST_RGST, TOTALPREM, CHLD_CNT, LTBN_CHLD_AGE, 

                                              MAX_PRM, CUST_INCM, RCBASE_HSHD_INCM, JPBASE_HSHD_INCM, POLY_NO, RECP_DATE, ORIG_RESN_DATE, RESN_DATE, HOSP_OTPA_STDT, HOSP_OTPA_ENDT,

                                              VLID_HOSP_OTDA, HOUSE_HOSP_DIST, DMND_AMT, PAYM_AMT, SELF_CHAM, NON_PAY, TAMT_SFCA,

                                              PATT_CHRG_TOTA, DSCT_AMT, COUNT_TRMT_ITEM, NON_PAY_RATIO, CNTT_YM, REAL_PAYM_TERM,

                                              EXPR_YM, EXTN_YM, LAPS_YM, MAIN_INSR_AMT, SUM_ORIG_PREM, MNTH_INCM_AMT, DISTANCE))

test1$SIU_CUST_YN<-0

str(test1)

pred<-predict(svm_model, test1)

test1<-na.omit(test1)

test1$pred<-pred


write.csv(test1, "SVM_TEST.csv")

summary(svm_model)


train_n_1<-train_n[1:20000, 1:40]

test_n_1<-train_n[20001:25000, 1:40]

test_n_1<-test_n_1[,-1]

str(test_n_1)

str(train_n_1)

svm_model_n1<-svm(train_n_1[,1] ~., data=train_n_1)

pred<-predict(svm_model_n1, test_n_1)

head(train[,62])

head(train_n$SIU_CUST_YN)

head(train_a)

head(train$POLY_NO)



# choose variables

train<-svm(train_n$SIU_CUST_YN ~ ., data=train_n)



	
####################################################################

# income prediction model  - PCA

DATA<-read.csv("DATA.csv", header=T)

INCM<-subset(DATA[,57:60])

summary(DATA$WEDD_YN)

INCM2<-na.omit(INCM)


INCM<-subset(DATA[,57:60])

pca<-prcomp(INCM2[,1:4])

summary(pca)

plot(prcomp(INCM2[,1:4]), type="l", sub = "Scree Plot")

INCM_pred<-predict(pca, INCM)

summary(INCM_pred)

write.csv(INCM_pred, "INCM_pca.csv")

INCM$INCM5<-mean(INCM[,1], INCM[,2])

TRAIN<- read.csv("BGCON_TRAIN.csv")

head(TRAIN)

INCM<-subset(TRAIN, select=c("CUST_ID", "RCBASE_HSHD_INCM","JPBASE_HSHD_INCM","CUST_INCM","MNTH_INCM_AMT"))

INCM[,2]<-INCM[,2]*10000 # change measure(Won)

INCM[,3]<-INCM[,3]*10000 # change measure(Won)

INCM[,4]<-INCM[,4]*10000 # change measure(Won)

INCM[,5]<-INCM[,5]*12 # change measure(per month => per year)

as.data.frame(INCM)

INCM$HOUSE_INCM<-0

summary(INCM)

length(INCM[,2])

for(i in 1:109773){

  if(INCM[i,2]==0){
	
    if(is.na(INCM[i,3])|| INCM[i,3]==0){
	
      na.omit(INCM[i,])
	
      }

    	INCM$HOUSE_INCM<-INCM[i,3]

    	}
	
  INCM$HOUSE_INCM<-(INCM[i,2]+INCM[i,3])/2
	
  i<-i+1

  }

head(INCM)

INCM$HOUSE_INCM	

INCM[1:100,]

INCM$HOUSE_INCM<-(INCM[,2]+INCM[,3])/2

na.omit(INCM)

str(INCM)

INCM$PERSON_INCM<-(INCM[,4]+INCM[,5])/2

INCM$HOUSE

if(is.na(INCM$PERSON_INCM)){
  
	INCM$PERSON_INCM<-INCM[,4]+INCM[,5]

	}

write.csv(INCM, "INCM_DATA.csv")


## clustering

INCM<-subset(TRAIN, select=c("CUST_ID", "RCBASE_HSHD_INCM","JPBASE_HSHD_INCM"))

INCM<-INCM[1:500,]

dist.eu <- dist(INCM, method="euclidean") 

res <- hclust(dist(INCM, method="maximum"), method="single")

plot(res, labels=c(1:10, 51:60))

INCM<-subset(DATA, select=c("INCM1", "INCM2", "INCM3", "INCM4"))

pred1<-lm(INCM2~INCM1+PREM, data=DATA)

DATA$INCM2_2<-predict(pred1, DATA)

for(n in 1:118962){

  	if(is.na(DATA[i,95])=="TRUE"){
	
  	  DATA[i,95]<-(2.731e+07)+(4.370e-01)*DATA$INCM1  + (1.527e-01)*DATA$PREM + (9.120e-02)*DATA$TOTAL_PREM

  	  }

  }	
	
if(is.na(DATA[,95])=="TRUE"){

  	DATA$INCM2_2<-(2.731e+07)+(4.370e-01)*DATA$INCM1  + (1.527e-01)*DATA$PREM + (9.120e-02)*DATA$TOTAL_PREM

  	}

summary(lm(INCM3~AGE, data=DATA)) 

summary(lm(INCM3~PREM, data=DATA)) 

summary(lm(INCM3~CRDT, data=DATA))

summary(lm(INCM3~AGE+PREM, data=DATA)) 

summary(lm(INCM3~AGE+PREM+CRDT, data=DATA))

summary(lm(INCM3~INCM2, data=DATA)) 

summary(lm(INCM3~AGE+PREM+CRDT+INCM2, data=DATA)) 


# income prediction

DATA<-read.csv("DATA.csv")

INCM3_TRAIN<-subset(DATA, select=c(AGE, PREM, CRDT, INCM2, INCM3))

INCM3_TRAIN<-na.omit(INCM3_TRAIN)

str(INCM3_TRAIN)

INCM3_TRAIN<-INCM3_TRAIN[1:10000,]

INCM3_SVM<-svm(INCM3 ~ ., data=INCM3_TRAIN)

INCM3_TEST<-subset(REAL_DATA, select=c(AGE, PREM, CRDT, INCM2, INCM3))

pred<-predict(INCM2_SVM,INCM2_TEST)

DATA_2<-subset(DATA, select=c(INCM1, INCM2, PREM, TOTAL_PREM))

DATA<-na.omit(DATA)

str(DATA)

INCM2_SVM<-svm(INCM2 ~ INCM1+PREM+PAYM_AMT+RESI_COST, data=DATA)

INCM3_SVM<-svm(INCM3 ~ AGE+PREM+CRDT+INCM2, data=DATA)

INCM4_SVM<-svm(INCM4 ~ INCM3+CUST_CLAIM+AGE+AGE_CL+CUST_CNTT, data=DATA)

INCM2_TRAIN<-subset(DATA, select=c(INCM1, PREM, PAYM_AMT, RESI_COST, INCM2))

INCM3_TRAIN<-subset(DATA, select=c(AGE, PREM, CRDT, INCM2, INCM3))

INCM4_TRAIN<-subset(DATA, select=c(INCM3, INCM4, CUST_CLAIM, AGE, AGE_CL, CUST_CNTT))

INCM2_TEST<-subset(REAL_DATA, select=c(INCM1, PREM, PAYM_AMT, RESI_COST, INCM2))

INCM2_TEST$INCM2<-0

INCM3_TEST<-subset(REAL_DATA, select=c(AGE, PREM, CRDT, INCM2, INCM3))

INCM3_TEST$INCM3<-0

INCM4_TEST<-subset(REAL_DATA, select=c(INCM3, INCM4, CUST_CLAIM, AGE, AGE_CL, CUST_CNTT))

INCM4_TEST$INCM4<-0

INCM2_TEST<-na.omit(INCM2_TEST)



INCM2_SVM<-svm(INCM2 ~ ., data=INCM2_TRAIN)

INCM3_SVM<-svm(INCM3 ~ ., data=INCM3_TRAIN)

INCM4_SVM<-svm(INCM4 ~ ., data=INCM4_TRAIN)

pred<-predict(INCM2_SVM,INCM2_TEST)

INCM2_TEST$SVM<-pred

write.csv(INCM2_TEST,"INCM2_REAL_FINAL.csv")

REAL_DATA$INCM2_SVM<-pred

REAL_DATA<-read.csv("DATA_FINAL.csv")

REAL_DATA<-as.data.frame(REAL_DATA)

summary(INCM2_SVM)

str(REAL_DATA)

a<-predict(INCM2_SVM, data=REAL_DATA)

REAL_DATA$INCM2_SVM<-a

pred<-predict(INCM2_SVM, REAL_DATA)

DATA$INCM3_SVM<-predict(INCM3_SVM, data=DATA)

DATA$INCM4_SVM<-predict(INCM4_SVM, data=DATA)

write.csv(DATA, "INCMDATA.csv")

OUTPUT<-read.csv("DATA.csv")

OUTPUT2<-subset(OUTPUT, select=c(INCM1,PREM,TOTAL_PREM,INCM2))

OUTPUT2$INCM2_SVM<-predict(INCM2_SVM, OUTPUT2)

DATA<-as.data.frame(DATA)

lm34<-lm(INCM3~INCM4, data=INCM)

lm12<-lm(INCM1~INCM2, data=INCM)

anova(lm(INCM3~INCM4, data=INCM))

lm1<-lm(INCM1~INCM2+INCM3+INCM4, data=INCM)

lm2<-lm(INCM2~INCM1+INCM3+INCM4, data=INCM)

lm3<-lm(INCM3~INCM1+INCM2+INCM4, data=INCM)

lm4<-lm(INCM4~INCM1+INCM2+INCM3, data=INCM)

plot(lm2)

INCM2<-lm(INCM2~AGE_CN + CUST_CNTT + PREM_MAX + TOTAL_PREM + PREM, data=DATA) 

plot(INCM2)

DATA<-as.data.frame(DATA)

INCM_pred<-predict(INCM2, DATA)

plot(INCM_pred, INCM$INCM2)


