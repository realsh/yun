setwd("C://DATA/CENTER")


# load table

TABLE1 <- read.csv("경강_계양김포.csv", header=TRUE)
TABLE2 <- read.csv("경강_고양.csv", header=TRUE)
TABLE3 <- read.csv("경강_구리남양주.csv", header=TRUE)
TABLE4 <- read.csv("경강_동수원.csv", header=TRUE)
TABLE5 <- read.csv("경강_부천부평.csv", header=TRUE)
TABLE6 <- read.csv("경강_분당판교.csv", header=TRUE)
TABLE7 <- read.csv("경강_서수원.csv", header=TRUE)
TABLE8 <- read.csv("경강_성남광주.csv", header=TRUE)
TABLE9 <- read.csv("경강_시흥.csv", header=TRUE)
TABLE10 <- read.csv("경강_안산.csv", header=TRUE)
TABLE11 <- read.csv("경강_안양.csv", header=TRUE)
TABLE12 <- read.csv("경강_영동춘천.csv", header=TRUE)
TABLE13 <- read.csv("경강_용인.csv", header=TRUE)
TABLE14 <- read.csv("경강_원주.csv", header=TRUE)
TABLE15 <- read.csv("경강_의정부.csv", header=TRUE)
TABLE16 <- read.csv("경강_인천남동.csv", header=TRUE)
TABLE17 <- read.csv("경강_인천남부.csv", header=TRUE)
TABLE18 <- read.csv("경강_인천서구.csv", header=TRUE)
TABLE19 <- read.csv("경강_청주.csv", header=TRUE)
TABLE20 <- read.csv("경강_충주제천.csv", header=TRUE)
TABLE21 <- read.csv("경강_평택화성.csv", header=TRUE)
TABLE22 <- read.csv("동부_경남김해.csv", header=TRUE)
TABLE23 <- read.csv("동부_경남서부.csv", header=TRUE)
TABLE24 <- read.csv("동부_경남양산.csv", header=TRUE)
TABLE25 <- read.csv("동부_경남중부.csv", header=TRUE)
TABLE26 <- read.csv("동부_경북동부.csv", header=TRUE)
TABLE27 <- read.csv("동부_경북북부.csv", header=TRUE)
TABLE28 <- read.csv("동부_대구남부.csv", header=TRUE)
TABLE29 <- read.csv("동부_대구달서.csv", header=TRUE)
TABLE30 <- read.csv("동부_대구동부.csv", header=TRUE)
TABLE31 <- read.csv("동부_대구북부.csv", header=TRUE)
TABLE32 <- read.csv("동부_대구서부.csv", header=TRUE)
TABLE33 <- read.csv("동부_대구수성.csv", header=TRUE)
TABLE34 <- read.csv("동부_부산동부.csv", header=TRUE)
TABLE35 <- read.csv("동부_부산북부.csv", header=TRUE)
TABLE36 <- read.csv("동부_부산사하.csv", header=TRUE)
TABLE37 <- read.csv("동부_부산중부.csv", header=TRUE)
TABLE38 <- read.csv("동부_울산중부.csv", header=TRUE)
TABLE39 <- read.csv("서부_광주광산.csv", header=TRUE)
TABLE40 <- read.csv("서부_광주남부.csv", header=TRUE)
TABLE41 <- read.csv("서부_광주북부.csv", header=TRUE)
TABLE42 <- read.csv("서부_광주서부.csv", header=TRUE)
TABLE43 <- read.csv("서부_대전남부.csv", header=TRUE)
TABLE44 <- read.csv("서부_대전북부.csv", header=TRUE)
TABLE45 <- read.csv("서부_대전서부.csv", header=TRUE)
TABLE46 <- read.csv("서부_대전중부.csv", header=TRUE)
TABLE47 <- read.csv("서부_세종.csv", header=TRUE)
TABLE48 <- read.csv("서부_여수.csv", header=TRUE)
TABLE49 <- read.csv("서부_익산군산.csv", header=TRUE)
TABLE50 <- read.csv("서부_전남동부.csv", header=TRUE)
TABLE51 <- read.csv("서부_전남서부.csv", header=TRUE)
TABLE52 <- read.csv("서부_전북서남.csv", header=TRUE)
TABLE53 <- read.csv("서부_천안.csv", header=TRUE)
TABLE54 <- read.csv("서부_충남남부.csv", header=TRUE)
TABLE55 <- read.csv("서부_충남서부.csv", header=TRUE)
TABLE56 <- read.csv("서부_충남중부.csv", header=TRUE)
TABLE57 <- read.csv("서부_충남중앙.csv", header=TRUE)
TABLE58 <- read.csv("서울_강남.csv", header=TRUE)
TABLE59 <- read.csv("서울_강동하남.csv", header=TRUE)
TABLE60 <- read.csv("서울_강북.csv", header=TRUE)
TABLE61 <- read.csv("서울_관악.csv", header=TRUE)
TABLE62 <- read.csv("서울_광진.csv", header=TRUE)
TABLE63 <- read.csv("서울_구로.csv", header=TRUE)
TABLE64 <- read.csv("서울_금천광명.csv", header=TRUE)
TABLE65 <- read.csv("서울_노원.csv", header=TRUE)
TABLE66 <- read.csv("서울_동대문.csv", header=TRUE)
TABLE67 <- read.csv("서울_성동.csv", header=TRUE)
TABLE68 <- read.csv("서울_성북.csv", header=TRUE)
TABLE69 <- read.csv("서울_송파.csv", header=TRUE)
TABLE70 <- read.csv("서울_은평.csv", header=TRUE)




#

TOTAL_N <- (nrow(TABLE1) + nrow(TABLE2) + nrow(TABLE3) + nrow(TABLE4) + nrow(TABLE5) +
              
              nrow(TABLE6) + nrow(TABLE7) + nrow(TABLE8) + nrow(TABLE9) + nrow(TABLE10) + 
              
              nrow(TABLE11) + nrow(TABLE12) + nrow(TABLE13) + nrow(TABLE14) + nrow(TABLE15) +
              
              nrow(TABLE16) + nrow(TABLE17) + nrow(TABLE18) + nrow(TABLE19) + nrow(TABLE20) + 
              
              nrow(TABLE21) + nrow(TABLE22) + nrow(TABLE23) + nrow(TABLE24) + nrow(TABLE25) +
              
              nrow(TABLE26) + nrow(TABLE27) + nrow(TABLE28) + nrow(TABLE29) + nrow(TABLE30) + 
              
              nrow(TABLE31) + nrow(TABLE32) + nrow(TABLE33) + nrow(TABLE34) + nrow(TABLE35) +
              
              nrow(TABLE36) + nrow(TABLE37) + nrow(TABLE38) + nrow(TABLE39) + nrow(TABLE40) + 
              
              nrow(TABLE41) + nrow(TABLE42) + nrow(TABLE43) + nrow(TABLE44) + nrow(TABLE45) +
              
              nrow(TABLE46) + nrow(TABLE47) + nrow(TABLE48) + nrow(TABLE49) + nrow(TABLE50) + 
              
              nrow(TABLE51) + nrow(TABLE52) + nrow(TABLE53) + nrow(TABLE54) + nrow(TABLE55) +
              
              nrow(TABLE56) + nrow(TABLE57) + nrow(TABLE58) + nrow(TABLE59) + nrow(TABLE60) + 
              
              nrow(TABLE61) + nrow(TABLE62) + nrow(TABLE63) + nrow(TABLE64) + nrow(TABLE65) +
              
              nrow(TABLE66) + nrow(TABLE67) + nrow(TABLE68) + nrow(TABLE69) + nrow(TABLE70) )

#


TOTAL_Varname_Set<-c(colnames(TABLE1), colnames(TABLE2),colnames(TABLE3),colnames(TABLE4),colnames(TABLE5),
                     
                     colnames(TABLE6), colnames(TABLE7),colnames(TABLE8),colnames(TABLE9),colnames(TABLE10),
                     
                     colnames(TABLE11), colnames(TABLE12),colnames(TABLE13),colnames(TABLE14),colnames(TABLE15),
                     
                     colnames(TABLE16), colnames(TABLE17),colnames(TABLE18),colnames(TABLE19),colnames(TABLE20),
                     
                     colnames(TABLE21), colnames(TABLE22),colnames(TABLE23),colnames(TABLE24),colnames(TABLE25),
                     
                     colnames(TABLE26), colnames(TABLE27),colnames(TABLE28),colnames(TABLE29),colnames(TABLE30),
                     
                     colnames(TABLE31), colnames(TABLE32),colnames(TABLE33),colnames(TABLE34),colnames(TABLE35),
                     
                     colnames(TABLE36), colnames(TABLE37),colnames(TABLE38),colnames(TABLE39),colnames(TABLE40),
                     
                     colnames(TABLE41), colnames(TABLE42),colnames(TABLE43),colnames(TABLE44),colnames(TABLE45),
                     
                     colnames(TABLE46), colnames(TABLE47),colnames(TABLE48),colnames(TABLE49),colnames(TABLE50),
                     
                     colnames(TABLE51), colnames(TABLE52),colnames(TABLE53),colnames(TABLE54),colnames(TABLE55),
                     
                     colnames(TABLE56), colnames(TABLE57),colnames(TABLE58),colnames(TABLE59),colnames(TABLE60),
                     
                     colnames(TABLE61), colnames(TABLE62),colnames(TABLE63),colnames(TABLE64),colnames(TABLE65),
                     
                     colnames(TABLE66), colnames(TABLE67),colnames(TABLE68),colnames(TABLE69),colnames(TABLE70))




TOTAL_VAR <- as.data.frame(summary.factor(TOTAL_Varname_Set))

write.csv(TOTAL_VAR, "TOTAL_VAR.csv") # 형식 맞춰서 다시 read

VAR_TABLE<-read.csv("TOTAL_VAR_edited.csv")

TOTAL_VAR_P<-as.data.frame(t(TOTAL_VAR))

length(TOTAL_VAR_P)




## result set making

for(i in (1:TOTAL_N)){
  for(j in (1:length(TOTAL_VAR_P))){
    # TOTAL_VAR_P[c(1:TOTAL_N),c(1:length(VAR_TABLE[,1]))] <- NA # 열: 변수개수 /  행: 전체데이터수
    TOTAL_VAR_P[i,j] <- NA
  }
}






VAR_TABLE <- VAR_TABLE[,c(2,3)]


colnames(TOTAL_VAR_P)

TABLE1_1<- as.data.frame(TABLE1)

for (i in (1:nrow(TABLE1_1))){
  for (j in (1:ncol(TABLE1_1))){
    if(is.na(TABLE1_1[i,j])==TRUE){
      TABLE1_1[i,j] <- 0 
    }
  }
}

is.na(TABLE1[5,1])





























############################################### start




TOTAL_VAR_1 <- TOTAL_VAR_P[c(1:length(TABLE1[,1])),]
colnames(TABLE1)

for (i in 1:length(TABLE1[,1])){
  for(j in 1:length(TOTAL_VAR_1))
    if((colnames(TABLE1)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_1[,j] <- TABLE1[,i]
    }
} # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




TOTAL_VAR_2 <- TOTAL_VAR_P[c(1:length(TABLE2[,1])),]


for (i in 1:length(TABLE2[,1])){
  for(j in 1:length(TOTAL_VAR_2))
    if((colnames(TABLE2)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_2[,j] <- TABLE2[,i]
    }
}


TOTAL_VAR_3 <- TOTAL_VAR_P[c(1:length(TABLE3[,1])),]


for (i in 1:length(TABLE3[,1])){
  for(j in 1:length(TOTAL_VAR_3))
    if((colnames(TABLE3)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_3[,j] <- TABLE3[,i]
    }
}

TOTAL_VAR_4 <- TOTAL_VAR_P[c(1:length(TABLE4[,1])),]


for (i in 1:length(TABLE4[,1])){
  for(j in 1:length(TOTAL_VAR_4))
    if((colnames(TABLE4)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_4[,j] <- TABLE4[,i]
    }
}


TOTAL_VAR_5 <- TOTAL_VAR_P[c(1:length(TABLE5[,1])),]


for (i in 1:length(TABLE5[,1])){
  for(j in 1:length(TOTAL_VAR_5))
    if((colnames(TABLE5)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_5[,j] <- TABLE5[,i]
    }
}


TOTAL_VAR_6 <- TOTAL_VAR_P[c(1:length(TABLE6[,1])),]


for (i in 1:length(TABLE6[,1])){
  for(j in 1:length(TOTAL_VAR_6))
    if((colnames(TABLE6)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_6[,j] <- TABLE6[,i]
    }
}


TOTAL_VAR_7 <- TOTAL_VAR_P[c(1:length(TABLE7[,1])),]


for (i in 1:length(TABLE7[,1])){
  for(j in 1:length(TOTAL_VAR_7))
    if((colnames(TABLE7)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_7[,j] <- TABLE7[,i]
    }
}


TOTAL_VAR_8 <- TOTAL_VAR_P[c(1:length(TABLE8[,1])),]


for (i in 1:length(TABLE8[,1])){
  for(j in 1:length(TOTAL_VAR_8))
    if((colnames(TABLE8)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_8[,j] <- TABLE8[,i]
    }
}


TOTAL_VAR_9 <- TOTAL_VAR_P[c(1:length(TABLE9[,1])),]


for (i in 1:length(TABLE9[,9])){
  for(j in 1:length(TOTAL_VAR_9))
    if((colnames(TABLE9)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_9[,j] <- TABLE9[,i]
    }
}


TOTAL_VAR_10 <- TOTAL_VAR_P[c(1:length(TABLE10[,1])),]


for (i in 1:length(TABLE10[,1])){
  for(j in 1:length(TOTAL_VAR_10))
    if((colnames(TABLE10)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_10[,j] <- TABLE10[,i]
    }
}



TOTAL_VAR_11 <- TOTAL_VAR_P[c(1:length(TABLE11[,1])),]


for (i in 1:length(TABLE11[,1])){
  for(j in 1:length(TOTAL_VAR_11))
    if((colnames(TABLE11)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_11[,j] <- TABLE11[,i]
    }
}



TOTAL_VAR_12 <- TOTAL_VAR_P[c(1:length(TABLE12[,1])),]


for (i in 1:length(TABLE12[,1])){
  for(j in 1:length(TOTAL_VAR_12))
    if((colnames(TABLE12)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_12[,j] <- TABLE12[,i]
    }
}



TOTAL_VAR_13 <- TOTAL_VAR_P[c(1:length(TABLE13[,1])),]


for (i in 1:length(TABLE13[,1])){
  for(j in 1:length(TOTAL_VAR_13))
    if((colnames(TABLE13)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_13[,j] <- TABLE13[,i]
    }
}



TOTAL_VAR_14 <- TOTAL_VAR_P[c(1:length(TABLE14[,1])),]


for (i in 1:length(TABLE14[,1])){
  for(j in 1:length(TOTAL_VAR_14))
    if((colnames(TABLE14)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_14[,j] <- TABLE14[,i]
    }
}



TOTAL_VAR_15 <- TOTAL_VAR_P[c(1:length(TABLE15[,1])),]


for (i in 1:length(TABLE15[,1])){
  for(j in 1:length(TOTAL_VAR_15))
    if((colnames(TABLE15)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_15[,j] <- TABLE15[,i]
    }
}



TOTAL_VAR_16 <- TOTAL_VAR_P[c(1:length(TABLE16[,1])),]


for (i in 1:length(TABLE16[,1])){
  for(j in 1:length(TOTAL_VAR_16))
    if((colnames(TABLE16)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_16[,j] <- TABLE16[,i]
    }
}



TOTAL_VAR_17 <- TOTAL_VAR_P[c(1:length(TABLE17[,1])),]


for (i in 1:length(TABLE17[,1])){
  for(j in 1:length(TOTAL_VAR_17))
    if((colnames(TABLE17)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_17[,j] <- TABLE17[,i]
    }
}



TOTAL_VAR_18 <- TOTAL_VAR_P[c(1:length(TABLE18[,1])),]


for (i in 1:length(TABLE18[,1])){
  for(j in 1:length(TOTAL_VAR_18))
    if((colnames(TABLE18)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_18[,j] <- TABLE18[,i]
    }
}



TOTAL_VAR_19 <- TOTAL_VAR_P[c(1:length(TABLE19[,1])),]


for (i in 1:length(TABLE19[,1])){
  for(j in 1:length(TOTAL_VAR_19))
    if((colnames(TABLE19)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_19[,j] <- TABLE19[,i]
    }
}

TOTAL_VAR_20 <- TOTAL_VAR_P[c(1:length(TABLE20[,1])),]


for (i in 1:length(TABLE20[,1])){
  for(j in 1:length(TOTAL_VAR_20))
    if((colnames(TABLE20)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_20[,j] <- TABLE20[,i]
    }
}


TOTAL_VAR_21 <- TOTAL_VAR_P[c(1:length(TABLE21[,1])),]


for (i in 1:length(TABLE21[,1])){
  for(j in 1:length(TOTAL_VAR_21))
    if((colnames(TABLE21)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_21[,j] <- TABLE21[,i]
    }
}


TOTAL_VAR_22 <- TOTAL_VAR_P[c(1:length(TABLE22[,1])),]


for (i in 1:length(TABLE22[,1])){
  for(j in 1:length(TOTAL_VAR_22))
    if((colnames(TABLE22)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_22[,j] <- TABLE22[,i]
    }
}


TOTAL_VAR_23 <- TOTAL_VAR_P[c(1:length(TABLE23[,1])),]


for (i in 1:length(TABLE23[,1])){
  for(j in 1:length(TOTAL_VAR_23))
    if((colnames(TABLE23)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_23[,j] <- TABLE23[,i]
    }
}


TOTAL_VAR_24 <- TOTAL_VAR_P[c(1:length(TABLE24[,1])),]


for (i in 1:length(TABLE24[,1])){
  for(j in 1:length(TOTAL_VAR_24))
    if((colnames(TABLE24)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_24[,j] <- TABLE24[,i]
    }
}


TOTAL_VAR_25 <- TOTAL_VAR_P[c(1:length(TABLE25[,1])),]


for (i in 1:length(TABLE25[,1])){
  for(j in 1:length(TOTAL_VAR_25))
    if((colnames(TABLE25)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_25[,j] <- TABLE25[,i]
    }
}


TOTAL_VAR_26 <- TOTAL_VAR_P[c(1:length(TABLE26[,1])),]


for (i in 1:length(TABLE26[,1])){
  for(j in 1:length(TOTAL_VAR_26))
    if((colnames(TABLE26)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_26[,j] <- TABLE26[,i]
    }
}


TOTAL_VAR_27 <- TOTAL_VAR_P[c(1:length(TABLE27[,1])),]


for (i in 1:length(TABLE27[,1])){
  for(j in 1:length(TOTAL_VAR_27))
    if((colnames(TABLE27)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_27[,j] <- TABLE27[,i]
    }
}


TOTAL_VAR_28 <- TOTAL_VAR_P[c(1:length(TABLE28[,1])),]


for (i in 1:length(TABLE28[,1])){
  for(j in 1:length(TOTAL_VAR_28))
    if((colnames(TABLE28)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_28[,j] <- TABLE28[,i]
    }
}


TOTAL_VAR_29 <- TOTAL_VAR_P[c(1:length(TABLE29[,1])),]


for (i in 1:length(TABLE29[,1])){
  for(j in 1:length(TOTAL_VAR_29))
    if((colnames(TABLE29)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_29[,j] <- TABLE29[,i]
    }
}



TOTAL_VAR_30 <- TOTAL_VAR_P[c(1:length(TABLE30[,1])),]


for (i in 1:length(TABLE30[,1])){
  for(j in 1:length(TOTAL_VAR_30))
    if((colnames(TABLE30)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_30[,j] <- TABLE30[,i]
    }
}



TOTAL_VAR_31 <- TOTAL_VAR_P[c(1:length(TABLE31[,1])),]


for (i in 1:length(TABLE31[,1])){
  for(j in 1:length(TOTAL_VAR_31))
    if((colnames(TABLE31)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_31[,j] <- TABLE31[,i]
    }
}



TOTAL_VAR_32 <- TOTAL_VAR_P[c(1:length(TABLE32[,1])),]


for (i in 1:length(TABLE32[,1])){
  for(j in 1:length(TOTAL_VAR_32))
    if((colnames(TABLE32)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_32[,j] <- TABLE32[,i]
    }
}



TOTAL_VAR_33 <- TOTAL_VAR_P[c(1:length(TABLE33[,1])),]


for (i in 1:length(TABLE33[,1])){
  for(j in 1:length(TOTAL_VAR_33))
    if((colnames(TABLE33)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_33[,j] <- TABLE33[,i]
    }
}



TOTAL_VAR_34 <- TOTAL_VAR_P[c(1:length(TABLE34[,1])),]


for (i in 1:length(TABLE34[,1])){
  for(j in 1:length(TOTAL_VAR_34))
    if((colnames(TABLE34)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_34[,j] <- TABLE34[,i]
    }
}



TOTAL_VAR_35 <- TOTAL_VAR_P[c(1:length(TABLE35[,1])),]


for (i in 1:length(TABLE35[,1])){
  for(j in 1:length(TOTAL_VAR_35))
    if((colnames(TABLE35)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_35[,j] <- TABLE35[,i]
    }
}



TOTAL_VAR_36 <- TOTAL_VAR_P[c(1:length(TABLE36[,1])),]


for (i in 1:length(TABLE36[,1])){
  for(j in 1:length(TOTAL_VAR_36))
    if((colnames(TABLE36)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_36[,j] <- TABLE36[,i]
    }
}

TOTAL_VAR_37 <- TOTAL_VAR_P[c(1:length(TABLE37[,1])),]


for (i in 1:length(TABLE37[,1])){
  for(j in 1:length(TOTAL_VAR_37))
    if((colnames(TABLE37)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_37[,j] <- TABLE37[,i]
    }
}


TOTAL_VAR_38 <- TOTAL_VAR_P[c(1:length(TABLE38[,1])),]


for (i in 1:length(TABLE38[,1])){
  for(j in 1:length(TOTAL_VAR_38))
    if((colnames(TABLE38)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_38[,j] <- TABLE38[,i]
    }
}


TOTAL_VAR_39 <- TOTAL_VAR_P[c(1:length(TABLE39[,1])),]


for (i in 1:length(TABLE39[,9])){
  for(j in 1:length(TOTAL_VAR_39))
    if((colnames(TABLE39)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_39[,j] <- TABLE9[,i]
    }
}


TOTAL_VAR_40 <- TOTAL_VAR_P[c(1:length(TABLE40[,1])),]


for (i in 1:length(TABLE40[,1])){
  for(j in 1:length(TOTAL_VAR_40))
    if((colnames(TABLE40)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_40[,j] <- TABLE40[,i]
    }
}

## 40


TOTAL_VAR_41 <- TOTAL_VAR_P[c(1:length(TABLE41[,1])),]


for (i in 1:length(TABLE41[,1])){
  for(j in 1:length(TOTAL_VAR_41))
    if((colnames(TABLE41)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_41[,j] <- TABLE41[,i]
    }
} # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




TOTAL_VAR_42 <- TOTAL_VAR_P[c(1:length(TABLE42[,1])),]


for (i in 1:length(TABLE42[,1])){
  for(j in 1:length(TOTAL_VAR_42))
    if((colnames(TABLE42)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_42[,j] <- TABLE42[,i]
    }
}


TOTAL_VAR_43 <- TOTAL_VAR_P[c(1:length(TABLE43[,1])),]


for (i in 1:length(TABLE43[,1])){
  for(j in 1:length(TOTAL_VAR_43))
    if((colnames(TABLE43)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_43[,j] <- TABLE43[,i]
    }
}

TOTAL_VAR_44 <- TOTAL_VAR_P[c(1:length(TABLE44[,1])),]


for (i in 1:length(TABLE44[,1])){
  for(j in 1:length(TOTAL_VAR_44))
    if((colnames(TABLE44)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_44[,j] <- TABLE44[,i]
    }
}


TOTAL_VAR_45 <- TOTAL_VAR_P[c(1:length(TABLE45[,1])),]


for (i in 1:length(TABLE45[,1])){
  for(j in 1:length(TOTAL_VAR_45))
    if((colnames(TABLE45)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_45[,j] <- TABLE45[,i]
    }
}


TOTAL_VAR_46 <- TOTAL_VAR_P[c(1:length(TABLE46[,1])),]


for (i in 1:length(TABLE46[,1])){
  for(j in 1:length(TOTAL_VAR_46))
    if((colnames(TABLE46)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_46[,j] <- TABLE46[,i]
    }
}


TOTAL_VAR_47 <- TOTAL_VAR_P[c(1:length(TABLE47[,1])),]


for (i in 1:length(TABLE47[,1])){
  for(j in 1:length(TOTAL_VAR_47))
    if((colnames(TABLE47)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_47[,j] <- TABLE47[,i]
    }
}


TOTAL_VAR_48 <- TOTAL_VAR_P[c(1:length(TABLE48[,1])),]


for (i in 1:length(TABLE48[,1])){
  for(j in 1:length(TOTAL_VAR_48))
    if((colnames(TABLE48)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_48[,j] <- TABLE48[,i]
    }
}


TOTAL_VAR_49 <- TOTAL_VAR_P[c(1:length(TABLE49[,1])),]


for (i in 1:length(TABLE49[,9])){
  for(j in 1:length(TOTAL_VAR_49))
    if((colnames(TABLE49)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_49[,j] <- TABLE49[,i]
    }
}

TOTAL_VAR_50 <- TOTAL_VAR_P[c(1:length(TABLE50[,1])),]


for (i in 1:length(TABLE50[,9])){
  for(j in 1:length(TOTAL_VAR_50))
    if((colnames(TABLE50)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_50[,j] <- TABLE50[,i]
    }
}


TOTAL_VAR_51 <- TOTAL_VAR_P[c(1:length(TABLE51[,1])),]


for (i in 1:length(TABLE51[,1])){
  for(j in 1:length(TOTAL_VAR_51))
    if((colnames(TABLE51)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_51[,j] <- TABLE51[,i]
    }
} # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




TOTAL_VAR_52 <- TOTAL_VAR_P[c(1:length(TABLE52[,1])),]


for (i in 1:length(TABLE52[,1])){
  for(j in 1:length(TOTAL_VAR_52))
    if((colnames(TABLE52)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_52[,j] <- TABLE52[,i]
    }
}


TOTAL_VAR_53 <- TOTAL_VAR_P[c(1:length(TABLE53[,1])),]


for (i in 1:length(TABLE53[,1])){
  for(j in 1:length(TOTAL_VAR_53))
    if((colnames(TABLE53)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_53[,j] <- TABLE53[,i]
    }
}

TOTAL_VAR_54 <- TOTAL_VAR_P[c(1:length(TABLE54[,1])),]


for (i in 1:length(TABLE54[,1])){
  for(j in 1:length(TOTAL_VAR_54))
    if((colnames(TABLE54)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_54[,j] <- TABLE54[,i]
    }
}


TOTAL_VAR_55 <- TOTAL_VAR_P[c(1:length(TABLE55[,1])),]


for (i in 1:length(TABLE55[,1])){
  for(j in 1:length(TOTAL_VAR_55))
    if((colnames(TABLE55)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_55[,j] <- TABLE55[,i]
    }
}


TOTAL_VAR_56 <- TOTAL_VAR_P[c(1:length(TABLE56[,1])),]


for (i in 1:length(TABLE56[,1])){
  for(j in 1:length(TOTAL_VAR_56))
    if((colnames(TABLE56)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_56[,j] <- TABLE56[,i]
    }
}


TOTAL_VAR_57 <- TOTAL_VAR_P[c(1:length(TABLE57[,1])),]


for (i in 1:length(TABLE57[,1])){
  for(j in 1:length(TOTAL_VAR_57))
    if((colnames(TABLE57)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_57[,j] <- TABLE57[,i]
    }
}


TOTAL_VAR_58 <- TOTAL_VAR_P[c(1:length(TABLE58[,1])),]


for (i in 1:length(TABLE58[,1])){
  for(j in 1:length(TOTAL_VAR_58))
    if((colnames(TABLE58)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_58[,j] <- TABLE58[,i]
    }
}


TOTAL_VAR_59 <- TOTAL_VAR_P[c(1:length(TABLE59[,1])),]


for (i in 1:length(TABLE59[,9])){
  for(j in 1:length(TOTAL_VAR_59))
    if((colnames(TABLE59)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_59[,j] <- TABLE59[,i]
    }
}

TOTAL_VAR_60 <- TOTAL_VAR_P[c(1:length(TABLE60[,1])),]


for (i in 1:length(TABLE60[,9])){
  for(j in 1:length(TOTAL_VAR_60))
    if((colnames(TABLE60)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_60[,j] <- TABLE60[,i]
    }
}


TOTAL_VAR_61 <- TOTAL_VAR_P[c(1:length(TABLE61[,1])),]


for (i in 1:length(TABLE61[,1])){
  for(j in 1:length(TOTAL_VAR_61))
    if((colnames(TABLE61)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_61[,j] <- TABLE61[,i]
    }
} # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




TOTAL_VAR_62 <- TOTAL_VAR_P[c(1:length(TABLE62[,1])),]


for (i in 1:length(TABLE62[,1])){
  for(j in 1:length(TOTAL_VAR_62))
    if((colnames(TABLE62)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_62[,j] <- TABLE62[,i]
    }
}


TOTAL_VAR_63 <- TOTAL_VAR_P[c(1:length(TABLE63[,1])),]


for (i in 1:length(TABLE63[,1])){
  for(j in 1:length(TOTAL_VAR_63))
    if((colnames(TABLE63)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_63[,j] <- TABLE63[,i]
    }
}

TOTAL_VAR_64 <- TOTAL_VAR_P[c(1:length(TABLE64[,1])),]


for (i in 1:length(TABLE64[,1])){
  for(j in 1:length(TOTAL_VAR_64))
    if((colnames(TABLE64)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_64[,j] <- TABLE64[,i]
    }
}


TOTAL_VAR_65 <- TOTAL_VAR_P[c(1:length(TABLE65[,1])),]


for (i in 1:length(TABLE65[,1])){
  for(j in 1:length(TOTAL_VAR_65))
    if((colnames(TABLE65)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_65[,j] <- TABLE65[,i]
    }
}


TOTAL_VAR_66 <- TOTAL_VAR_P[c(1:length(TABLE66[,1])),]


for (i in 1:length(TABLE66[,1])){
  for(j in 1:length(TOTAL_VAR_66))
    if((colnames(TABLE66)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_66[,j] <- TABLE66[,i]
    }
}


TOTAL_VAR_67 <- TOTAL_VAR_P[c(1:length(TABLE67[,1])),]


for (i in 1:length(TABLE67[,1])){
  for(j in 1:length(TOTAL_VAR_67))
    if((colnames(TABLE67)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_67[,j] <- TABLE67[,i]
    }
}


TOTAL_VAR_68 <- TOTAL_VAR_P[c(1:length(TABLE68[,1])),]


for (i in 1:length(TABLE68[,1])){
  for(j in 1:length(TOTAL_VAR_68))
    if((colnames(TABLE68)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_68[,j] <- TABLE68[,i]
    }
}


TOTAL_VAR_69 <- TOTAL_VAR_P[c(1:length(TABLE69[,1])),]


for (i in 1:length(TABLE69[,9])){
  for(j in 1:length(TOTAL_VAR_69))
    if((colnames(TABLE69)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_69[,j] <- TABLE69[,i]
    }
}

TOTAL_VAR_70 <- TOTAL_VAR_P[c(1:length(TABLE70[,1])),]


for (i in 1:length(TABLE70[,9])){
  for(j in 1:length(TOTAL_VAR_70))
    if((colnames(TABLE70)[i]==VAR_TABLE[j,1])==TRUE){
      TOTAL_VAR_70[,j] <- TABLE70[,i]
    }
}




TOTAL <- rbind(TOTAL_VAR_1, TOTAL_VAR_2, TOTAL_VAR_3, TOTAL_VAR_4, TOTAL_VAR_5, 
               
               TOTAL_VAR_6, TOTAL_VAR_7, TOTAL_VAR_8, TOTAL_VAR_9, TOTAL_VAR_10, 
               
               TOTAL_VAR_11, TOTAL_VAR_12, TOTAL_VAR_13, TOTAL_VAR_14, TOTAL_VAR_15, 
               
               TOTAL_VAR_16, TOTAL_VAR_17, TOTAL_VAR_18, TOTAL_VAR_19, TOTAL_VAR_20, 
               
               TOTAL_VAR_21, TOTAL_VAR_22, TOTAL_VAR_23, TOTAL_VAR_24, TOTAL_VAR_25, 
               
               TOTAL_VAR_26, TOTAL_VAR_27, TOTAL_VAR_28, TOTAL_VAR_29, TOTAL_VAR_30, 
               
               TOTAL_VAR_31, TOTAL_VAR_32, TOTAL_VAR_33, TOTAL_VAR_34, TOTAL_VAR_35, 
               
               TOTAL_VAR_36, TOTAL_VAR_37, TOTAL_VAR_38, TOTAL_VAR_39, TOTAL_VAR_40,
               
               TOTAL_VAR_41, TOTAL_VAR_42, TOTAL_VAR_43, TOTAL_VAR_44, TOTAL_VAR_45, 
               
               TOTAL_VAR_46, TOTAL_VAR_47, TOTAL_VAR_48, TOTAL_VAR_49, TOTAL_VAR_50,
               
               TOTAL_VAR_51, TOTAL_VAR_52, TOTAL_VAR_53, TOTAL_VAR_54, TOTAL_VAR_55, 
               
               TOTAL_VAR_56, TOTAL_VAR_57, TOTAL_VAR_58, TOTAL_VAR_59, TOTAL_VAR_60,
               
               TOTAL_VAR_61, TOTAL_VAR_62, TOTAL_VAR_63, TOTAL_VAR_64, TOTAL_VAR_65, 
               
               TOTAL_VAR_66, TOTAL_VAR_67, TOTAL_VAR_68, TOTAL_VAR_69, TOTAL_VAR_70)

write.csv(TOTAL, "OUTPUT.csv")
