library(ggplot2)
library(dplyr)
library(car)
library(plyr)
library(quickregression)
library(tidyr)
library(dumm)
library(xgboost)
library(rpivotTable)
library(party)

house_train=read.csv("housing_train.csv",stringsAsFactors = F,na.strings = c("","NA","unknown"))
house_test=read.csv("housing_test.csv",stringsAsFactors = F,na.strings = c("","NA","unknown"))
str(house_train)
str(house_test)

house_train$Price1=house_train$Price
house_train$Price=NULL
names(house_train)[16]="Price"
house_train$is_train=1

house_test$Price=0
house_test$is_train=0


house=rbind(house_train,house_test)
str(house)

a=sapply (house$Address, function(x) tail(strsplit(x,split=" ")[[1]],1))
View(a)



a=as.matrix(a)
table(a)
length(house$road %in% names(sort(table(a),decreasing = T))[1:8])
colSums(is.na(house))
house$road=(a)
house$road=as.character(house$road)

house$road[! house$road %in% names(sort(table(a),decreasing = T))[1:8]]="others"
table(house$road)

house$shouse= grepl("/", house$Address)
house$shouse=ifelse(house$shouse=="TRUE",1,0)
# house$Suburb=NULL
house$Address=NULL
#house$SellerG=NULL


table(house$Bedroom2)
house[is.na(house$Bedroom2),c("Bedroom2")]=3
# house[house$Bedroom2>5,c("Bedroom2")]=3


# xtabs(Rooms~Bathroom+Type+Bedroom2,data=house)
table(house$Bathroom)


house[is.na(house$Bathroom),c("Bathroom")]=1
# house[house$Bathroom>4,c("Bathroom")]=1



table(house$Car)
house[is.na(house$Car),c("Car")]=1
# house[house$Car>4,c("Car")]=1


# 

#========"Landsize"==========


hist(house$Landsize)
house$Landsize[is.na(house$Landsize)]=median(house$Landsize,na.rm = T)
house$Landsize=log(house$Landsize+1)


##===BuildingArea NA removal========



hist(house$BuildingArea)
house$BuildingArea[is.na(house$BuildingArea)]=median(house$BuildingArea,na.rm = T)
house$BuildingArea=log(house$BuildingArea+1)


##===YearBuilt NA removal========

sort(table(house$YearBuilt),decreasing = T)
house[is.na(house$YearBuilt),"YearBuilt"]=1970




##===CouncilArea NA removal========
sort(table(house$CouncilArea),decreasing = T)

# house$CouncilArea[is.na(house$CouncilArea)]="Boroondara"


house$Suburb=gsub("[[:punct:]]","",house$Suburb)
house$Suburb=gsub("[^[:alnum:]]","",house$Suburb)

house$CouncilArea=gsub("[[:punct:]]","",house$CouncilArea)
house$CouncilArea=gsub("[^[:alnum:]]","",house$CouncilArea)

house$SellerG=gsub("[[:punct:]]","",house$SellerG)
house$SellerG=gsub("[^[:alnum:]]","",house$SellerG)





##=====  Outliers=====


View(house)

View(house[house$BuildingArea>house$Landsize & house$Landsize!=0,])

# house=house %>% mutate(tot_room = Rooms+Bedroom2+Bathroom+Car) %>% select(-Rooms,-Bedroom2,-Bathroom,-Car)


# house$Landsize[grep("/",house$Address)] 


str(house)
hist(house$Price)
b=house
str(b)
# b=b[-c(5,9)]
colSums(is.na(b))
# b$CouncilArea=NULL
b$SellerG=NULL
b$Suburb=NULL
b$YearBuilt=NULL
b$Postcode=NULL
b$CouncilArea=as.factor(b$CouncilArea)



b=dummy(b,c("Type","Method","road"),limit = 500,rm_original = T)
# b=dummy(b,c("YearBuilt","Suburb","Type","Method","Postcode","road"),limit = 500,rm_original = T)

dim(b)

house_train=b[b$is_train==1,]
house_train$is_train=NULL

house_test=b[b$is_train==0,]
house_test$is_train=NULL


b=house_train
set.seed(2)
a1=sample(1:nrow(b), nrow(b)*0.8)
b_train=b[a1,]
b_test=b[-a1,]
dim(b_test)
str(b_train)
sum(colSums(is.na(b_train)))

a7=names(b_train)[c(-1,-2,-6,-7)]														               # Start of VIF check formula creation
a4=as.numeric(which(a7 == "Price"))
a5=paste0("+",a7[-a4],collapse = "")
a5=substr(a5,2,nchar(a5))
a5
# a1=substr(a5,start = 32,stop = nchar(a5))
# a1

ctrl <- mob_control(alpha = 0.05, bonferroni = TRUE, minsplit = 10,
                    objfun = deviance, verbose = TRUE)

b_train$Distance=log(b_train$Distance+1)
b_train$Landsize=log(b_train$Landsize+1)
b_train$BuildingArea=log(b_train$BuildingArea+1)
b_train$Price=log(b_train$Price)
b_train$Room=as.factor(b_train$Room)
b_train$Bathroom=as.factor(b_train$Bathroom)
b_train$Car=as.factor(b_train$Car)

fmBH <- mob(Price ~ Distance + Landsize+BuildingArea+Rooms   | Bathroom+Car+shouse+Type_h+Type_u+Method_S+Method_PI+Method_SP+Method_VB+road_St+road_Rd+road_Av+road_others+road_Cr+road_Ct+road_Gr+road_Dr,
            data = b_train, control = ctrl, model = linearModel)

fmBH
plot(fmBH, node = 3)
coef(fmBH)
summary(fmBH, node = 7)
sctest(fmBH, node = 7)
sqrt(mean(residuals(fmBH)^2))
logLik(fmBH)
AIC(fmBH)

pre=predict(fmBH,b_train)
plot((b_train$Price),(pre))
plot(exp(b_train$Price),exp(pre))
sqrt(mean((exp(b_train$Price)-exp(pre))^2))
