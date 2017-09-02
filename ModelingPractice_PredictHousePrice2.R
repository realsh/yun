
setwd("C://")

guro45=read.csv("guro45.csv")

str(guro45)

install.packages("scatterplot3d")

library(scatterplot3d)

library(car)


scatterplot3d(guro45$x, guro45$y, guro45$price, type="h", pch=16, angle=50, highlight.3d=TRUE, box=TRUE, col.axis="blue", grid=TRUE, col.grid="gray", mar=c(3,4,4,3),

              xlab="X of apartment", ylab="Y of apartment",zlab="Z of apartment", main="3 dimension scatter plot of apartments"))

# subset - feature selection

guro1=subset(guro45, select=c(type, year, area, story, porch, households, brand, PpH, highest, lowest,

                              dis_office, dis_sub, dis_danji, dis_kiji, prison, price))
str(guro1)


# add PpA

guro1$PpA=guro1$price/guro1$area


# multiple linear regression(1)

gurolin=lm(price~., data=guro1)

summary(gurolin)

## p-value�� ���� ��� ��ȿ���� ���� ����: highest, lowest, dis_office(����)

guro2=subset(guro1, select=c(type, year, area, story, porch, households, brand, PpH,

                             dis_sub, dis_danji, dis_kiji, prison, price))

head(guro3)

# multiple linear regression(2)

gurolin2=lm(price~., data=guro2)

summary(gurolin2)

vif(gurolin2)

# princomp linear regression

#1) ��� 0, ǥ������ 1�� ���� / ������ ����(double-external)�� ä��

guro3<-scale(guro2)

guro4=subset(guro3,select=c(dis_sub, dis_danji, dis_kiji, prison, price))

head(guro4)

#2) �ּ��� �м� - �ּ��� ���ھ� Ȯ�� �� scree plot

pca<-prcomp(guro4[,1:4]) # �ܺ��� ����

print(pca)

summary(pca)

plot(prcomp(guro4[,1:4]), type="l", sub = "Scree Plot")

# 85%�� �л��� �����ϴ� �� 2�ּ��б��� ���

pca2<-prcomp(guro3[,1:8]) # ������ ����

print(pca2)

summary(pca2)

plot(prcomp(guro3[,1:8]), type="l", sub = "Scree Plot")

# 91.6%�� �л��� �����ϴ� �� 6�ּ��б��� ���

head(pca2)

head(pca2)

#3) �ּ��� ȸ�� �м�

pc_x1<-pca$x[,1]

pc_x2<-pca$x[,2]

pc_x3<-pca2$x[,1]

pc_x4<-pca2$x[,2]

pc_x5<-pca2$x[,3]

pc_x6<-pca2$x[,4]

pc_x7<-pca2$x[,5]

pc_x8<-pca2$x[,6]

price<-guro4[,5]



head(guro4)


# �ּ��� ����� ���������� ���� ���ο� data set ����

guro5<-cbind(pc_x1, pc_x2, pc_x3, pc_x4, pc_x5, pc_x6, pc_x7, pc_x8, price)

guro5<-data.frame(guro5)

head(guro5)

as.dataframe(guro5)

# linear regression

gurolin3=lm(price~., data=guro5)

summary(gurolin3)





