setwd("C://")

guro45=read.csv("guro45.csv")

str(guro45)

install.packages("scatterplot3d")

library(scatterplot3d)


# subset - variables

guro1=subset(guro45, select=c(dong, year, area, price, story, porch,type,households,brand,PpH,highest,lowest, prison, dis_sub, dis_danji, dis_office, dis_ic, dis_kiji))

str(guro1)

# add PpA

# add dong.numeric, making dong into numeric

guro1$PpA=guro1$price/guro1$area

guro1$dong.numeric=as.numeric(guro1$dong)

str(guro1$dong.numeric)


summary(guro1)

str(guro1)

# correlations for house related features

cor(guro1$price, guro1$area) #0.728

cor(guro1$price, guro1$type) #0.015

cor(guro1$price, guro1$porch) #0.489

cor(guro1$price, guro1$households) #0.223

cor(guro1$price, guro1$brand) #0.287

cor(guro1$price, guro1$type) #0.015

cor(guro1$price, guro1$PpH) #0.502

cor(guro1$price, guro1$highest) #0.410

cor(guro1$price, guro1$lowest) #0.335

cor(guro1$dis_danji, guro1$dis_kiji) #0.869

plot(guro1$dis_kiji, guro1$price)


# correlations for prison distance

cor(guro1$price, guro1$prison)

# 0.2563501

plot(guro1$prison, guro1$price)

# this means a positive correlation betwen prison distance and price (which is correct)

# add prison_sq: dont add just use I(prison^2)

# guro1$prison_sq=guro1$prison^2

# cor(guro1$price, guro1$prison_sq) # 0.264

# plot(guro1$price, guro1$prison_sq)

# remove prison_sq for now

# guro1$prison_sq=NULL


# correlation for price and subway distance

cor(guro1$price, guro1$dis_sub)

# -0.1591405

# also correct



## linear model: find minimal adquate model

str(guro1)

gurolin=lm(price~.-dong -PpA, data=guro1) # all except dong and PpA

summary(gurolin)

# Residual standard error: 3953 on 1019 degrees of freedom

# Multiple R-squared:  0.8501,  Adjusted R-squared:  0.8473 

# F-statistic: 304.2 on 19 and 1019 DF,  p-value: < 2.2e-16

# lowest : highest p value


# 2nd try

gurolin1=update(gurolin, .~. -lowest)

summary(gurolin1)

# Residual standard error: 3953 on 1020 degrees of freedom

# Multiple R-squared:   0.85,  Adjusted R-squared:  0.8474 

# F-statistic: 321.2 on 18 and 1020 DF,  p-value: < 2.2e-16

# -> lower R squared and adj. R squared, higher F stat but only slightly

# dis_danji: highest p value



# 3rd try

gurolin2=update(gurolin1, .~. -y)

summary(gurolin2)

# Residual standard error: 3952 on 1021 degrees of freedom

# Multiple R-squared:  0.8499,  Adjusted R-squared:  0.8475 

# F-statistic: 340.2 on 17 and 1021 DF,  p-value: < 2.2e-16


# -> a bit of improvement compared to gurolin1

# all variables significant: end here



## now try multicolinearity

# correlation matrix

pairs(guro1)

# vif over 2.5 is problematic, try with gurolin (no variables removed)

library(car)

vif(gurolin)

# dis_danji highest, so remove

gurolin3=update(gurolin, .~. -dis_danji)

vif(gurolin3)

# dis_office highest, so remove

gurolin4=update(gurolin3, .~. -dis_office)

vif(gurolin4)

# x highest, so remove

# prison=15.221499, but cannot remove this, so remove digital_danji

gurolin5=update(gurolin4, .~. -x)

vif(gurolin5)

# dis_kiji highest, so remove

gurolin6=update(gurolin5, .~. -dis_kiji)

vif(gurolin6)

# all variables vif under 3; stop here

#look at stats for gurolin6

summary(gurolin6)

# Residual standard error: 4227 on 1023 degrees of freedom

# Multiple R-squared:  0.828,  Adjusted R-squared:  0.8255 

# F-statistic: 328.3 on 15 and 1023 DF,  p-value: < 2.2e-16

# highest and lowest still insignificant, remove these

gurolin7=update(gurolin6, .~. -highest)

summary(gurolin7)

gurolin8=update(gurolin7, .~. -lowest)

summary(gurolin8)

gurolin8_=update(gurolin8, .~. -dis_ic -type)

summary(gurolin8_)

# Residual standard error: 4223 on 1025 degrees of freedom

# Multiple R-squared:  0.828,  Adjusted R-squared:  0.8258 

# F-statistic: 379.4 on 13 and 1025 DF,  p-value: < 2.2e-16

# comment on results: R squared and adj. have gone down (compared to gurolin), but F test and t test values better

# try adding  back dong variable and removing dong.numeric

str(guro1)


gurolin9=update(gurolin8, .~. +dong-dong.numeric)

summary(gurolin9)

# Residual standard error: 4075 on 1018 degrees of freedom

# Multiple R-squared:  0.8409,  Adjusted R-squared:  0.8378 

# F-statistic:   269 on 20 and 1018 DF,  p-value: < 2.2e-16

gurolin10=update(gurolin9, .~. -dong)

summary(gurolin10)

# comment: stats have improved so this is final model

# stats improve when dong is used as factor (not sure why)


# look at correlation between prison, dis_danji, dis_office, dis_kiji, x, highest, lowest for confirmation

cor(guro1$prison, guro1$x)

# 0.9148765

cor(guro1$prison, guro1$y)

# 0.4361488

cor(guro1$prison, guro1$dis_danji)

# -0.7277859

cor(guro1$prison, guro1$dis_office)

# -0.8188371

cor(guro1$prison, guro1$dis_kiji)

# -0.5079187






# Load CART packages

library(rpart)

library(rpart.plot)

# CART model

str(guro1)

latlontree = rpart(price ~. -dong -PpA -dis_danji -dis_office - dis_ic- dis_kiji -prison_sq, data=guro45)

summary(latlontree)

windows()

prp(latlontree)

# prison: 3.5km



## rattle

install.packages("rattle")

library(rattle)

fancyRpartPlot(latlontree)

stepAIC(gurolin10, direction="both") 

stepAIC(gurolin1, direction="both") 
