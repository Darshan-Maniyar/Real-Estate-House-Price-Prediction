library(ggplot2)
library(dplyr)
library(car)
library(plyr)
library(quickregression)
library(tidyr)
library(dumm)
library(xgboost)
library(rpivotTable)
windows()
# rpivotTable(house, rows="Bedroom2", col="Bedroom2", aggregatorName="Average", 
# vals="Price", rendererName="Treemap")
house=read.csv("housing_train.csv",stringsAsFactors = F,na.strings = c("","NA","unknown"))
str(house)
colSums(is.na(house))

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
# table(house$Rooms)
# house[house$Rooms>6,c("Rooms")]=3
# 
# table(house$Rooms)
# house[house$Rooms>4 & house$Type=="u",c("Rooms")]=3
# 
# table(house$Rooms)
# house[house$Rooms>4 & house$Type=="t",c("Rooms")]=3
# 

#========"Landsize"==========

# house$Landsize[house$Landsize==0 ]=NA

# house[house$Landsize>1283,"Landsize"]=median(house$Landsize,na.rm = T)

# house$Landsize=(house$Landsize)^0.5

hist(house$Landsize)
house$Landsize[is.na(house$Landsize)]=median(house$Landsize,na.rm = T)

# hist((house$Landsize)^0.5)
# boxplot((house$Landsize))

##===BuildingArea NA removal========



hist(house$BuildingArea)
house$BuildingArea[is.na(house$BuildingArea)]=median(house$BuildingArea,na.rm = T)
# house[house$BuildingArea>295,"BuildingArea"]=median(house$BuildingArea,na.rm = T)
# hist((house$BuildingArea)^0.5)

# boxplot(house$BuildingArea)

##===YearBuilt NA removal========

sort(table(house$YearBuilt),decreasing = T)
house[is.na(house$YearBuilt),"YearBuilt"]=1970

# for(i in 1 : round(((max(house$YearBuilt)-min(house$YearBuilt))/10),0)){
#   print(i)
#   print(min(house$YearBuilt)+((i)*10))
#   house$yearb[house$YearBuilt>=(min(house$YearBuilt)+((i-1)*10)) & house$YearBuilt<(min(house$YearBuilt)+((i)*10))]= i
#   
# }

# house=house[,-c(15)]
##===CouncilArea NA removal========
sort(table(house$CouncilArea),decreasing = T)

house$CouncilArea[is.na(house$CouncilArea)]="Boroondara"


house$Suburb=gsub("[[:punct:]]","",house$Suburb)
house$Suburb=gsub("[^[:alnum:]]","",house$Suburb)

house$CouncilArea=gsub("[[:punct:]]","",house$CouncilArea)
house$CouncilArea=gsub("[^[:alnum:]]","",house$CouncilArea)

house$SellerG=gsub("[[:punct:]]","",house$SellerG)
house$SellerG=gsub("[^[:alnum:]]","",house$SellerG)

# for(i in 1 : ncol(house)){
#   if(class(house[,i])=="character"){
#     house[,i]=as.factor(house[,i])
#   }
# }



##=====  Outliers=====


View(house)

View(house[house$BuildingArea>house$Landsize & house$Landsize!=0,])

house=house %>% mutate(tot_room = Rooms+Bedroom2+Bathroom+Car) %>% select(-Rooms,-Bedroom2,-Bathroom,-Car)


# house$Landsize[grep("/",house$Address)] 





str(house)
hist(house$Price)
b=house
str(b)
b=b[-c(1,2,5,6,8,9,10,11)]
colSums(is.na(b))


b=dummy(b,c("Type","CouncilArea"),limit = 200,rm_original = T)


# dmy=dummyVars("~.",b)
# b=data.frame(predict(dmy,b))
# a=which(names(b) %in%"Price")
# b[-a]=scale(b[-a])
set.seed(2)
a1=sample(1:nrow(b), nrow(b)*0.70)
b_train=b[a1,]
b_test=b[-a1,]
dim(b_test)




##==============LM===================

fit=qlm(data = b_train,V_dependent = Price)
summary(fit)

plot(fit)

bt=b_train

bt$pre=predict(fit,bt)
hist(bt$pre)

bt$res=bt$Price-bt$pre

ggplot(bt,aes(pre,res))+geom_point()+geom_smooth()

ggplot(bt,aes(pre,Price))+geom_point()+geom_smooth()


sqrt(mean((bt$res)^2))

##=====  2Outliers=====


View(house)

View(house[house$BuildingArea>house$Landsize & house$Landsize!=0,])

house=house %>% mutate(tot_room = Rooms+Bedroom2+Bathroom+Car) %>% select(-Rooms,-Bedroom2,-Bathroom,-Car)


# house$Landsize[grep("/",house$Address)] 





str(house)
hist(house$Price)
b=house
str(b)
b=b[-c(1,2,5,6,8,11)]
colSums(is.na(b))


b=dummy(b,c("Type","CouncilArea"),limit = 200,rm_original = T)


# dmy=dummyVars("~.",b)
# b=data.frame(predict(dmy,b))
# a=which(names(b) %in%"Price")
# b[-a]=scale(b[-a])
set.seed(2)
a1=sample(1:nrow(b), nrow(b)*0.70)
b_train=b[a1,]
b_test=b[-a1,]
dim(b_test)




##==============2LM===================

fit=qlm(data = b_train,V_dependent = Price)
summary(fit)

plot(fit)
View(bt[415,])

bt=b_train

bt$pre=predict(fit,bt)
hist(bt$pre)

bt$res=bt$Price-bt$pre

ggplot(bt,aes(pre,res))+geom_point()+geom_smooth()

ggplot(bt,aes(BuildingArea,Price))+geom_point()+geom_smooth()


sqrt(mean((bt$res)^2))


##=====  3Outliers=====


View(house)

View(house[house$BuildingArea>house$Landsize & house$Landsize!=0,])

house=house %>% mutate(tot_room = Rooms+Bedroom2+Bathroom+Car) %>% select(-Rooms,-Bedroom2,-Bathroom,-Car)


# house$Landsize[grep("/",house$Address)] 





str(house)
hist(house$Price)
b=house
str(b)
b=b %>% mutate(quality = BuildingArea/tot_room) 

b=b[-c(1,2,5,6,8,11)]
colSums(is.na(b))


b=dummy(b,c("Type","CouncilArea"),limit = 200,rm_original = T)


b$Distance2=b$Distance^2
b$Landsize2=b$Landsize^2
b$BuildingArea2=b$BuildingArea^2
b$tot_room2=b$tot_room^2

# dmy=dummyVars("~.",b)
# b=data.frame(predict(dmy,b))
# a=which(names(b) %in%"Price")
# b[-a]=scale(b[-a])
set.seed(2)
a1=sample(1:nrow(b), nrow(b)*0.70)
b_train=b[a1,]
b_test=b[-a1,]
dim(b_test)




##==============3LM===================

fit=qlm(data = b_train,V_dependent = Price)
summary(fit)

plot(fit)
View(bt[415,])

bt=b_train

bt$pre=predict(fit,bt)
hist(bt$pre)

bt$res=bt$Price-bt$pre

ggplot(bt,aes(pre,res))+geom_point()+geom_smooth()

ggplot(bt,aes(pre,Price))+geom_point()+geom_smooth()


sqrt(mean((bt$res)^2))

##===========Test===================

house=read.csv("housing_test.csv",stringsAsFactors = F,na.strings = c("","NA","unknown"))
str(house)
colSums(is.na(house))

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
# table(house$Rooms)
# house[house$Rooms>6,c("Rooms")]=3
# 
# table(house$Rooms)
# house[house$Rooms>4 & house$Type=="u",c("Rooms")]=3
# 
# table(house$Rooms)
# house[house$Rooms>4 & house$Type=="t",c("Rooms")]=3
# 

#========"Landsize"==========

# house$Landsize[house$Landsize==0 ]=NA

# house[house$Landsize>1283,"Landsize"]=median(house$Landsize,na.rm = T)

# house$Landsize=(house$Landsize)^0.5

hist(house$Landsize)
house$Landsize[is.na(house$Landsize)]=median(house$Landsize,na.rm = T)

hist((house$Landsize)^0.5)
boxplot((house$Landsize))

##===BuildingArea NA removal========



hist(house$BuildingArea)
house$BuildingArea[is.na(house$BuildingArea)]=median(house$BuildingArea,na.rm = T)
# house[house$BuildingArea>295,"BuildingArea"]=median(house$BuildingArea,na.rm = T)
hist((house$BuildingArea)^0.5)

boxplot(house$BuildingArea)

##===YearBuilt NA removal========

sort(table(house$YearBuilt),decreasing = T)
house[is.na(house$YearBuilt),"YearBuilt"]=1970

# for(i in 1 : round(((max(house$YearBuilt)-min(house$YearBuilt))/10),0)){
#   print(i)
#   print(min(house$YearBuilt)+((i)*10))
#   house$yearb[house$YearBuilt>=(min(house$YearBuilt)+((i-1)*10)) & house$YearBuilt<(min(house$YearBuilt)+((i)*10))]= i
#   
# }

# house=house[,-c(15)]
##===CouncilArea NA removal========
sort(table(house$CouncilArea),decreasing = T)

house$CouncilArea[is.na(house$CouncilArea)]="Boroondara"


house$Suburb=gsub("[[:punct:]]","",house$Suburb)
house$Suburb=gsub("[^[:alnum:]]","",house$Suburb)

house$CouncilArea=gsub("[[:punct:]]","",house$CouncilArea)
house$CouncilArea=gsub("[^[:alnum:]]","",house$CouncilArea)

house$SellerG=gsub("[[:punct:]]","",house$SellerG)
house$SellerG=gsub("[^[:alnum:]]","",house$SellerG)

b=house
str(b)
b=b[-c(2,6,8,14)]
colSums(is.na(b))


b=dummy(b,c("Suburb","Type","Method","CouncilArea"),limit = 200,rm_original = T)


# dmy=dummyVars("~.",b)
# b=data.frame(predict(dmy,b))
a=which(names(b) %in%"Price")
b=scale(b)
b_test=b
dim(b_test)

b5=as.matrix(b_test)

xgbdtest=xgb.DMatrix(data=b5)

bt1=b5
bt1$pred=predict(bst,xgbdtest,ntreelimit =214 )#as.matrix(h1_test[2:425])
hist(bt1$pred)

write.csv(bt1$pred, file = "xgb.csv",row.names = F)
