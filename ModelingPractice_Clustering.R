## kang-won BIGTORY competition

setwd("C://")

DATA<-read.csv("KW_DATA.csv")

DATA<-na.omit(DATA)

## plotting

install.packages("scatterplot3d")

library(scatterplot3d)

scatterplot3d(DATA$X, DATA$Y, DATA$CAMERA, type="h", pch=16, angle=50, 

              highlight.3d=TRUE, box=TRUE, col.axis="blue", grid=TRUE, col.grid="gray", 

              mar=c(3,4,4,3),xlab="LON of CAM", ylab="LAT of CAM",zlab="NUM of CAM", 

              main="3 dimension scatter plot of CAM")



scatterplot3d::scatterplot3d

range(DATA$CAMERA)


## make subset(location, number of crime)  

CCTV<-subset(DATA, select=c("X","Y","CRIME1"))

## re-scaling(X, Y)

CCTV$X<-CCTV$X * 1000000

CCTV$Y<-CCTV$Y * 1000000

head(CCTV)

plot(CCTV$X, CCTV$Y)

XY<-subset(CCTV, select=c("X","Y"))

summary(res)

str(res)

head(DATA$CRIME1)

mulreg<-lm(DATA$CRIME1~DATA$CAMERA)

summary(mulreg)

plot(mulreg)

res <- hclust(dist(CCTV, method="euclidean"), method="single") # n=11

plot(res)

res <- hclust(dist(CCTV, method="euclidean"), method="average") # n=11

plot(res)

res <- hclust(dist(CCTV, method="euclidean"), method="complete") # n=11

plot(res)

NU_DATA<-subset(DATA, select=c(-DAY))

head(res2)

res2<-hclust(dist(DATA, method="euclidean"), method="average")

grouping<-kmeans(XY,11)

##	 $centers
##           X        Y
##	1  129144593 37436792
##	2  128736169 37333588
##	3  127839462 37268744
##	4  127668107 38086983
##	5  127732410 37869123
##	6  127508794 37406755
##	7  127964791 37352709
##	8  127563922 37432335
##	9  128894115 37768613
##	10 128126558 38056091
##	11 128535334 38272072



## visualization 

install.packages("googleVis")

library(googleVis)

demo(WorldBank)

googleVis::gvisMap

str(DATA)

head(DATA)

CAM<-gvisMotionChart(DATA,idvar="LOCATION", timevar="X") ## failed

## Trying google map

## LATLON making

head(DATA)

DATA$Y

LATLON<-DATA$Y+":"+DATA$X

cctv<-gvisMap(DATA, "LATLON", "LOCATION",

              	options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
	
                mapType='normal', useMapTypeControl=TRUE, width=1000, height=400))

plot(cctv)


##	1  129144593 37436792
##	2  128736169 37333588
##	3  127839462 37268744
##	4  127668107 38086983
##	5  127732410 37869123
##	6  127508794 37406755
##	7  127964791 37352709
##	8  127563922 37432335
##	9  128894115 37768613
##	10 128126558 38056091
##	11 128535334 38272072

cent1_lon<-37.436792

cent1_lat<-129.144593

cent2_lon<-37.333588

cent2_lat<-128.736169

cent3_lon<-37.268744


cent3_lat<-127.839462

cent4_lon<-38.086983

cent4_lat<-127.668107

cent5_lon<-37.869123

cent5_lat<-127.732410

cent6_lon<-37.406755

cent6_lat<-127.508794

cent7_lon<-37.352709

cent7_lat<-127.964791

cent8_lon<-37.432335

cent8_lat<-127.563922

cent9_lon<-37.768613

cent9_lat<-128.894115

cent10_lon<-38.056091

cent10_lat<-128.126558

cent11_lon<-38.272072

cent11_lat<-128.535334

cent1<-rbind(cent1_lon,cent1_lat)

dist(XY, method="euclidean")

DATA$c1_dist<-((DATA$X-cent1_lat)^2 + (DATA$Y_cent1_lon)^2)^0.5



CCTV<-read.csv("CCTV.csv", header=TRUE)

centroid<-gvisMap(CCTV, "LATLON", "name",

                  	options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
	
                  	mapType='normal', useMapTypeControl=TRUE, width=1000, height=400))

plot(centroid)


