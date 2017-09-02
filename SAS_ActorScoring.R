## load data


setwd("C://DATA/RAW_DATA")


CONTENTS_META_RAW.dt<-read.csv("CONTENTS_META.csv")

setwd("C://DATA/RAW_DATA/v3")

CONTENTS_META.dt<-read.csv("CONTENTS_META_V3.csv")

CUSTOMER.dt<-read.csv("CUSTOMER_V3.csv")

HISTORY_DETAIL.dt<-read.csv("HISTORY_DETAIL_V3.csv")

HISTORY.dt<-read.csv("HISTORY_V3.csv")

SEARCH.dt<-read.csv("SEARCH_V3.csv")

str(CONTENTS_META.dt)

setwd("C://DATA")

MOVIE<-read.csv("CONT_MISSING.csv")

MOVIE_CONTENTS_META.dt <- CONTENTS_META.dt[,c(1:16,20)]

MOVIE_CONTENTS_META.dt<-subset(MOVIE_CONTENTS_META.dt, MOVIE_CONTENTS_META.dt$contents_category=="movie")
# 10111개

MOVIE_ACTOR<-CONTENTS_META_RAW.dt[,c(1,13,24)]

MOVIE_CONTENTS_META.dt<- merge(MOVIE_CONTENTS_META.dt, MOVIE_ACTOR, by='album_id', all.x=TRUE)

MOVIE_CONTENTS_META.dt<- merge(MOVIE_CONTENTS_META.dt, MOVIE, by='album_id', all.x=TRUE)

# library

library(data.table)

library(tm)



## scoring Model 1 - 배우

setwd("C://DATA/SCORING/ACTOR")

write.csv(MOVIE_CONTENTS_META.dt, "MOVIE_TOTAL.csv")

KOR_MOVIE_CONTENTS_META.dt<-subset(MOVIE_CONTENTS_META.dt, MOVIE_CONTENTS_META.dt$country=="한국")
# 1030개

FOR_MOVIE_CONTENTS_META.dt<-subset(MOVIE_CONTENTS_META.dt, MOVIE_CONTENTS_META.dt$country!="한국")
# 9081개



for(i in 1:length(KOR_MOVIE_CONTENTS_META.dt$actors_display)){
  if(is.na(KOR_MOVIE_CONTENTS_META.dt[i,18])==TRUE){
    KOR_MOVIE_CONTENTS_META.dt[i,18] <- KOR_MOVIE_CONTENTS_META.dt[i,19]
  }
} # actors_display가 비어있으면 starring_actor 집어넣기


for(i in 1:length(FOR_MOVIE_CONTENTS_META.dt$actors_display)){
  if(is.na(FOR_MOVIE_CONTENTS_META.dt[i,18])==TRUE){
    FOR_MOVIE_CONTENTS_META.dt[i,18] <- FOR_MOVIE_CONTENTS_META.dt[i,19]
  }
} 

summary(KOR_MOVIE_CONTENTS_META.dt$actors_display) # null 5

summary(FOR_MOVIE_CONTENTS_META.dt$actors_display) # null 1165

write.csv(KOR_MOVIE_CONTENTS_META.dt, "KOR_ACTOR.csv")

write.csv(FOR_MOVIE_CONTENTS_META.dt, "FOR_ACTOR.csv")

KOR_MOVIE_CONTENTS_META_2.dt<-read.csv("KOR_ACTOR.csv")

FOR_MOVIE_CONTENTS_META_2.dt<-read.csv("FOR_ACTOR.csv")

setwd("C://DATA/SCORING/ACTOR")


########################################################## ㅇㅕ기까지 했는데 sum_sale,sum_audience 채우고다시 할것
my.docs <- VectorSource(KOR_MOVIE_CONTENTS_META_2.dt$actors_display)

my.corpus <- Corpus(my.docs)

inspect(my.corpus)

term.doc.matrix.stm<- TermDocumentMatrix(my.corpus)

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

length(term.doc.matrix[,1030])

SCORE.dt<-KOR_MOVIE_CONTENTS_META_2.dt[,c(22,23)]

KOR_ACTOR_SCORE<-term.doc.matrix %*% as.matrix(SCORE.dt)

write.csv(KOR_ACTOR_SCORE,"KOR_ACTOR_SCORE.csv")

write.csv(t(KOR_ACTOR_SCORE),"KOR_ACTOR_SCORE_RE.csv")



# solve(term.doc.matrix) %*% 

actor_dt_sales<-matrix(KOR_ACTOR_SCORE[,1], ncol=1287, nrow=1287)

write.csv(t(actor_dt_sales),"FINAL_SCORE_COPY.csv")

actor_dt_aud<-matrix(KOR_ACTOR_SCORE[,2], ncol=1287, nrow=1287)

write.csv(t(actor_dt_aud),"FINAL_SCORE_COPY2.csv")

head(KOR_ACTOR_SCORE)

head(actor_dt_sales)

head(KOR_ACTOR_SCORE)

length(solve(term.doc.matrix))

write.csv(term.doc.matrix,"KOR_ACTOR_TDM.csv")

write.csv(t(term.doc.matrix),"KOR_ACTOR_DTM.csv")

####

KOR_NEW_VAR_SALES <- t(term.doc.matrix) %*% actor_dt_sales

KOR_NEW_VAR_SALES<- as.data.table(KOR_NEW_VAR_SALES)

write.csv(KOR_NEW_VAR_SALES,"NEW_v_KOR_SALES.csv")

KOR_NEW_VAR_SALES <- t(term.doc.matrix) %*% actor_dt_sales






my.docs <- VectorSource(FOR_MOVIE_CONTENTS_META_2.dt$actors_display)

my.corpus <- Corpus(my.docs)

inspect(my.corpus)

term.doc.matrix.stm<- TermDocumentMatrix(my.corpus)

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

length(term.doc.matrix[,1])

SCORE_F.dt<-FOR_MOVIE_CONTENTS_META_2.dt[,c(22,23)]

FOR_ACTOR_SCORE<-term.doc.matrix %*% as.matrix(SCORE_F.dt)

write.csv(FOR_ACTOR_SCORE,"FOR_ACTOR_SCORE.csv")

setwd("C://DATA/SCORING/ACTOR")

FOR_ACTOR_SCORE <- read.csv("FOR_ACTOR_SCORE.csv")

write.csv(term.doc.matrix,"FOR_ACTOR_TDM.csv")

write.csv(t(term.doc.matrix),"FOR_ACTOR_DTM.csv")

FOR_DTM<-as.data.frame(t(term.doc.matrix))

FOR_ACTOR_SCORE_df<-as.data.frame(FOR_ACTOR_SCORE)

str(FOR_ACTOR_SCORE[,1])

FOR_ACTOR_SCORE_s <-FOR_ACTOR_SCORE_df[,2]

FOR_ACTOR_SCORE_a <-FOR_ACTOR_SCORE_df[,3]

length(FOR_ACTOR_SCORE_df[,1])

FOR_ACTOR_SCORE_copy<-matrix(FOR_ACTOR_SCORE_s, nrow=4309, ncol=9081)

FOR_ACTOR_SCORE_copy_a<-matrix(FOR_ACTOR_SCORE_a, nrow=4309, ncol=9081)


setwd("C://DATA/SCORING/ACTOR")

FOR_ACOTR_DTM <- read.csv("FOR_ACTOR_DTM.csv")

FOR_ACOTR_DTM <- FOR_ACOTR_DTM[,2:4310]

write.csv(t(FOR_ACTOR_SCORE_copy),"FOR_ACTOR_SALECOPY.csv")

write.csv(t(FOR_ACTOR_SCORE_copy_a),"FOR_ACTOR_AUDPY.csv")

RESULT_FOR<- FOR_ACOTR_DTM * t(FOR_ACTOR_SCORE_copy)

RESULT_FOR_a<- FOR_ACOTR_DTM * t(FOR_ACTOR_SCORE_copy_a)

setwd("C://DATA/SCORING/ACTOR")

write.csv(RESULT_FOR, "FOR_SALES_SCORE_FIN.csv")

write.csv(RESULT_FOR_a, "FOR_AUD_SCORE_FIN.csv")

RESULT_FOR_edit<-RESULT_FOR

summary(RESULT_FOR_edit[1,])



## scoring Model 2 - 감독 



setwd("C://DATA/SCORING/DIRECTOR")

KOR_MOVIE_CONTENTS_META_D.dt<- merge(KOR_MOVIE_CONTENTS_META.dt, KOR_MOVIE_CONTENTSCONTENTS_META_2[,c(1,22,23)], by='album_id', all.x=TRUE)
# 1030
##################################아몰라ㅏㅏㅏ

FOR_MOVIE_CONTENTS_META_D.dt<- merge(FOR_MOVIE_CONTENTS_META.dt, CONTENTS_META_RAW.dt[,c(1,15,16)], by='album_id', all.x=TRUE)
# 9081



for(i in 1:length(KOR_MOVIE_CONTENTS_META_D.dt$actors_display)){
  if(is.na(KOR_MOVIE_CONTENTS_META_D.dt[i,23])==TRUE){
    KOR_MOVIE_CONTENTS_META_D.dt[i,23] <- KOR_MOVIE_CONTENTS_META_D.dt[i,24]
  }
}

summary(FOR_MOVIE_CONTENTS_META_D.dt$director)

for(i in 1:length(FOR_MOVIE_CONTENTS_META_D.dt$actors_display)){
  if(is.na(FOR_MOVIE_CONTENTS_META_D.dt[i,23])==TRUE){
    FOR_MOVIE_CONTENTS_META_D.dt[i,23] <- FOR_MOVIE_CONTENTS_META_D.dt[i,24]
  }
} 



my.docs <- VectorSource(KOR_MOVIE_CONTENTS_META_D.dt$director)

my.corpus <- Corpus(my.docs)

inspect(my.corpus)

term.doc.matrix.stm<- TermDocumentMatrix(my.corpus)

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

length(term.doc.matrix[,1])

SCORE_KOR_D.dt<-KOR_MOVIE_CONTENTS_META_2.dt[,c(22,23)]

str(SCORE_KOR_D.dt)

KOR_DIRECTOR_SCORE<-term.doc.matrix %*% as.matrix(SCORE_KOR_D.dt)

write.csv(KOR_DIRECTOR_SCORE,"KOR_DIRECTOR_SCORE.csv")

write.csv(term.doc.matrix,"KOR_DIRECTOR_TDM.csv")

write.csv(t(term.doc.matrix),"KOR_DIRECTOR_DTM.csv")

write.csv(KOR_MOVIE_CONTENTS_META_D.dt,"KOR_DIR_NAME.csv")


KOR_DTM_d<-as.data.frame(t(term.doc.matrix))

KOR_DIRECTOR_SCORE_df<-as.data.frame(KOR_DIRECTOR_SCORE)

KOR_DIRECTOR_SCORE_s <-KOR_DIRECTOR_SCORE_df[,1]

KOR_DIRECTOR_SCORE_a <-KOR_DIRECTOR_SCORE_df[,2]

length(KOR_DIRECTOR_SCORE_s)

length(KOR_DIRECTOR_SCORE_df[,1])

KOR_DIRECTOR_SCORE_copy<-matrix(KOR_DIRECTOR_SCORE_s, nrow=625, ncol=1030)

KOR_DIRECTOR_SCORE_copy_a<-matrix(KOR_DIRECTOR_SCORE_a, nrow=625, ncol=1030)



RESULT_KOR_D<- KOR_DTM_d * t(KOR_DIRECTOR_SCORE_copy)


RESULT_KOR_D_a<- KOR_DTM_d * t(KOR_DIRECTOR_SCORE_copy_a)


setwd("C://DATA/SCORING/DIRECTOR")

write.csv(RESULT_KOR_D, "KOR_SALES_FIN.csv")

write.csv(RESULT_KOR_D_a, "KOR_AUD_FIN.csv")







FOR_MOVIE_CONTENTS_META_D.dt$director <- gsub(" ", "", FOR_MOVIE_CONTENTS_META_D.dt$director)

write.csv(FOR_MOVIE_CONTENTS_META_D.dt,"MID_FOR_D.csv")



setwd("C://DATA/SCORING/DIRECTOR")

FOR_MOVIE_CONTENTS_META_D.dt<- read.csv("MID_FOR_D.csv")

my.docs <- VectorSource(FOR_MOVIE_CONTENTS_META_D.dt$director)

my.corpus <- Corpus(my.docs)

inspect(my.corpus)

term.doc.matrix.stm<- TermDocumentMatrix(my.corpus)

term.doc.matrix <- as.matrix(term.doc.matrix.stm)

SCORE_FOR_D.dt<-FOR_MOVIE_CONTENTS_META_D.dt[,c(22,23)]

str(SCORE_FOR_D.dt)

FOR_DIRECTOR_SCORE<-term.doc.matrix %*% as.matrix(SCORE_FOR_D.dt)

write.csv(FOR_DIRECTOR_SCORE,"FOR_DIRECTOR_SCORE.csv")

write.csv(term.doc.matrix,"FOR_DIRECTOR_TDM.csv")

write.csv(t(term.doc.matrix),"FOR_DIRECTOR_DTM.csv")



FOR_DTM_d<-as.data.frame(t(term.doc.matrix))

FOR_DIRECTOR_SCORE_df<-as.data.frame(FOR_DIRECTOR_SCORE)
str(FOR_DIRECTOR_SCORE_df)

FOR_DIRECTOR_SCORE_s <-FOR_DIRECTOR_SCORE_df[,1]
str(FOR_DIRECTOR_SCORE_s)

FOR_DIRECTOR_SCORE_a <-FOR_DIRECTOR_SCORE_df[,2]
str(FOR_DIRECTOR_SCORE_a)

length(FOR_DIRECTOR_SCORE_s)

length(FOR_DIRECTOR_SCORE_df[1,])

FOR_DIRECTOR_SCORE_copy<-matrix(FOR_DIRECTOR_SCORE_s, nrow=2612, ncol=9081)
write.csv(t(FOR_DIRECTOR_SCORE_copy),"FOR_DIR_SCORE_COPY.csv")

SALE_DIR<- t(FOR_DIRECTOR_SCORE_copy) * FOR_DTM_d

write.csv(SALE_DIR, "FOR_SALE_FIN.csv")


FOR_DIRECTOR_SCORE_copy_a<-matrix(FOR_DIRECTOR_SCORE_a, nrow=2612, ncol=9081)
write.csv(t(FOR_DIRECTOR_SCORE_copy_a),"FOR_DIR_SCORE_COPY_a.csv")

AUD_DIR<- t(FOR_DIRECTOR_SCORE_copy_a) * FOR_DTM_d

write.csv(AUD_DIR, "FOR_AUD_FIN.csv")

RESULT_FOR_D<- FOR_DTM_d * FOR_DIRECTOR_SCORE_copy



RESULT_FOR_D_a<- FOR_DTM_d * FOR_DIRECTOR_SCORE_copy_a

setwd("C://DATA/SCORING/DIRECTOR")

write.csv(RESULT_FOR_D, "FOR_SALES_SCORE_DIRECTOR.csv")

write.csv(RESULT_FOR_D_a, "FOR_AUD_SCORE_DIRECTOR.csv")





## plot check

setwd("C://DATA/RAW_DATA/v3.1")

CONT_MISSING<- read.csv("CONT_MISSING.csv")

KOR<- read.csv("NEW_VAR_KOR.csv")

KOR_2<- read.csv("NEW_VAR_KOR_D.csv")

FOR<- read.csv("NEW_VAR_FOR.csv")

FOR_2<- read.csv("NEW_VAR_FOR_D.csv")

CONT<-rbind(KOR,FOR)

CONT2<-rbind(KOR_2,FOR_2)

TOTAL_TABLE<-merge(CONT_MISSING,CONT, by='album_id', all=TRUE)

TOTAL_TABLE<-merge(TOTAL_TABLE,CONT2,by='album_id',all=TRUE)

write.csv(TOTAL_TABLE,"CHECK.csv")

TOTAL_TABLE<-merge(TOTAL_TABLE,CONTENTS_META_RAW.dt[,1:3],by='album_id',all.x=TRUE)

read.csv("")

