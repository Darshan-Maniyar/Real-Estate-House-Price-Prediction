library(ggplot2)
library(dplyr)
library(car)
library(plyr)
library(quickregression)
library(tidyr)
library(dumm)
library(xgboost)
library(rpivotTable)
library(neuralnet)
library( nnet)
windows()
rpivotTable(house, rows="Bedroom2", col="Bedroom2", aggregatorName="Average",
vals="Price", rendererName="Treemap")

house=read.csv("housing_train.csv",stringsAsFactors = F,na.strings = c("","NA","unknown"))
str(house)
colSums(is.na(house))

table(house$Bedroom2)
house[is.na(house$Bedroom2),c("Bedroom2")]=3
house[house$Bedroom2==0,c("Bedroom2")]=3


# xtabs(Rooms~Bathroom+Type+Bedroom2,data=house)
table(house$Bathroom)


house[is.na(house$Bathroom),c("Bathroom")]=1
house[house$Bathroom==0,c("Bathroom")]=1



table(house$Car)
house[is.na(house$Car),c("Car")]=1
house[house$Car==0,c("Car")]=1

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



# house$Landsize=(house$Landsize)^0.5

hist(house$Landsize)
house$Landsize[is.na(house$Landsize)]=median(house$Landsize,na.rm = T)

house[house$Landsize>2000,"Landsize"]=329


# hist((house$Landsize)^0.5)
# boxplot((house$Landsize))

##===BuildingArea NA removal========



hist(house$BuildingArea)
house$BuildingArea[is.na(house$BuildingArea)]=median(house$BuildingArea,na.rm = T)
house[house$BuildingArea>500,"BuildingArea"]=350


house[house$BuildingArea<40,"BuildingArea"]=40

house[house$Landsize<40,"Landsize"]=house[house$Landsize<40,"BuildingArea"]

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


# View(house)
# 
# View(house[house$BuildingArea>house$Landsize & house$Landsize!=0,])

house=house %>% mutate(tot_room = Rooms+Bedroom2+Bathroom+Car) %>% select(-Rooms,-Bedroom2,-Bathroom,-Car)

# house$Type[house$Type=="t" & house$Price>2000000]="h"
# 
# house$Type[house$Type=="u" & house$Price>2000000]="h"

# house$Landsize[grep("/",house$Address)] 

plot(house$tot_room[house$Price>3500000],house$Price[house$Price>3500000])

# house=house[-which(house$tot_room>20 & house$Price<2000000),]

house$Type_1=0
house$Type_1[grep("/",house$Address)]=1

house$Type_2=0
house$Type_2=sapply(strsplit( house$Address, "\\s+"), "[", 3)

a=sapply(strsplit( house$Address, "\\s+"), "[", 4)

house$Type_2[a=="St" & !is.na(a)]=a[a=="St" & !is.na(a)]

house$Type_2[a=="Rd" & !is.na(a)]=a[a=="Rd" & !is.na(a)]

house$Type_2[a=="Av" & !is.na(a)]=a[a=="Av" & !is.na(a)]

house$Type_2[house$Type_2=="Avenue"]="Av"
house$Type_2[house$Type_2=="Cir"]=" Cr"
house$Type_2[house$Type_2=="Cl"]="Cr"
table(house$Type_2)

k=unique(house$Type_2[house$Type_2 %in% names(table(house$Type_2)[table(house$Type_2)<100])])

house$Type_2[house$Type_2 %in% k]="other"
# house$Type_2[house$Type_2 %in% names(table(house$Type_2)[table(house$Type_2)<100])]="other"

# table(sapply(strsplit( house$Address, "\\s+"), "[", 4))


a1=aggregate(house$Price,by=list(Suburb=house$Suburb),mean)
# View(a)
a1$no=0

for(m in 1 : nrow(a1)){
  
for(i in 1 : 20){
  l=(100000*i)+300000
  
  if(a1$x[m] >l & a1$x[m]<l+100000  ) {
    a1$no[m]=i
  }
}
}

house$sub_grp=0

for(m in 1 : nrow(a1)){
 house$sub_grp[house$Suburb==a1$Suburb[m]]=a1$no[m]
}

table(house$sub_grp)

str(house)
hist(house$Price)
b=house
b=b[b$Price<3500000,]
str(b)
# b=b[-c(1,2,6,8)]
# b=b[-c(2,7,9,15)]
b=b[-c(1,2,6,8,11)]


colSums(is.na(b))
max(b$Price)
b$Landsize=sqrt(b$Landsize)
b$BuildingArea=sqrt(b$BuildingArea)

# b=dummy(b,c("Type","Suburb","CouncilArea","Method"),limit = 200,rm_original = T)
b=dummy(b,c("Type","Type_2","CouncilArea","Method","sub_grp"),limit = 200,rm_original = T)


# dmy=dummyVars("~.",b)
# b=data.frame(predict(dmy,b))
# a=which(names(b) %in%"Price")
# b[-a]=scale(b[-a])
set.seed(2)
a1=sample(1:nrow(b), nrow(b)*0.70)
b_train=b[a1,]
b_test=b[-a1,]
str(b_train)



##==============nn===================

# n <- names(b_train)
# f <- as.formula(paste("Price ~", paste(n[!n %in% "Price"], collapse = " + ")))
# nn <- neuralnet(f,data=b_train,hidden=c(5,3),linear.output=T)
set.seed(1)
sampidx <- c(sample(1:5266, 2500))

set.seed(1)
nn <- nnet(Price/3470000~.,data=b_train,size = 10,decay=0.05, maxit=1000,linout=T)#,subset = sampidx

summary(nn)
plot(nn)

bt=b_train
bt=b_test#[1:500,]

# View(b_test[2:5,])

bt$pre=predict(nn,bt,type = "raw")*3470000
hist(bt$pre)
hist(bt$Price)

bt$res=bt$Price-bt$pre

ggplot(bt,aes(Price,res))+geom_point()+geom_smooth()

ggplot(bt,aes(pre,Price))+geom_point()+geom_smooth()

sqrt(mean((bt$res)^2))#[bt$res<2500000 & bt$res>-2500000]
sqrt(mean((bt$res[bt$res<2500000 & bt$res>-2500000])^2))#

bt=cbind(bt$pre,bt$res,bt)
View(bt)
View(bt[bt$Price>3000000,])

library(NeuralNetTools)

resultOlden <- olden(nn, "Price/3470000", bar_plot=FALSE)
a=data.frame(resultOlden)
a$name=rownames(a)
str(a)
View(a)
a1=a[order(abs(a$importance)),]
a1=a1[1:20,]
a1=t(a1)
ggplot(a1,aes(importance))+geom_bar()

# require(devtools)
# source_gist('6206737')
# gar.fun('x',nn)
# 
# source_url('https://gist.githubusercontent.com/fawda123/6206737/raw/d6f365c283a8cae23fb20892dc223bc5764d50c7/gar_fun.r')

##=====  Test=====


house=read.csv("housing_test.csv",stringsAsFactors = F,na.strings = c("","NA","unknown"))
str(house)
colSums(is.na(house))

table(house$Bedroom2)
house[is.na(house$Bedroom2),c("Bedroom2")]=3
house[house$Bedroom2==0,c("Bedroom2")]=3


# xtabs(Rooms~Bathroom+Type+Bedroom2,data=house)
table(house$Bathroom)


house[is.na(house$Bathroom),c("Bathroom")]=1
house[house$Bathroom==0,c("Bathroom")]=1



table(house$Car)
house[is.na(house$Car),c("Car")]=1
house[house$Car==0,c("Car")]=1

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



# house$Landsize=(house$Landsize)^0.5

hist(house$Landsize)
house$Landsize[is.na(house$Landsize)]=329

house[house$Landsize>2000,"Landsize"]=329


# hist((house$Landsize)^0.5)
# boxplot((house$Landsize))

##===BuildingArea NA removal========



hist(house$BuildingArea)
house$BuildingArea[is.na(house$BuildingArea)]=121
house[house$BuildingArea>500,"BuildingArea"]=350


house[house$BuildingArea<40,"BuildingArea"]=40

house[house$Landsize<40,"Landsize"]=house[house$Landsize<40,"BuildingArea"]

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


# View(house)
# 
# View(house[house$BuildingArea>house$Landsize & house$Landsize!=0,])

house=house %>% mutate(tot_room = Rooms+Bedroom2+Bathroom+Car) %>% select(-Rooms,-Bedroom2,-Bathroom,-Car)

# house$Type[house$Type=="t" & house$Price>2000000]="h"
# 
# house$Type[house$Type=="u" & house$Price>2000000]="h"

# house$Landsize[grep("/",house$Address)] 

# plot(house$tot_room[house$Price>3500000],house$Price[house$Price>3500000])

# house=house[-which(house$tot_room>20 & house$Price<2000000),]

house$Type_1=0
house$Type_1[grep("/",house$Address)]=1

house$Type_2=0
house$Type_2=sapply(strsplit( house$Address, "\\s+"), "[", 3)

a=sapply(strsplit( house$Address, "\\s+"), "[", 4)

house$Type_2[a=="St" & !is.na(a)]=a[a=="St" & !is.na(a)]

house$Type_2[a=="Rd" & !is.na(a)]=a[a=="Rd" & !is.na(a)]

house$Type_2[a=="Av" & !is.na(a)]=a[a=="Av" & !is.na(a)]

house$Type_2[house$Type_2=="Avenue"]="Av"
house$Type_2[house$Type_2=="Cir"]=" Cr"
house$Type_2[house$Type_2=="Cl"]="Cr"
table(house$Type_2)

house$Type_2[house$Type_2 %in% k]="other"

# table(sapply(strsplit( house$Address, "\\s+"), "[", 4))


house$sub_grp=0

for(m in 1 : nrow(a1)){
  house$sub_grp[house$Suburb==a1$Suburb[m]]=a1$no[m]
}

table(house$sub_grp)
table(house$Type_2)

str(house)
b=house

str(b)
# b=b[-c(1,2,6,8)]
# b=b[-c(2,7,9,15)]
b=b[-c(1,2,5,7,10)]


colSums(is.na(b))

b$Landsize=sqrt(b$Landsize)
b$BuildingArea=sqrt(b$BuildingArea)

# b=dummy(b,c("Type","Suburb","CouncilArea","Method"),limit = 200,rm_original = T)
b=dummy(b,c("Type","Method","CouncilArea","Type_2","sub_grp"),limit = 200,rm_original = T)

b$pre=predict(nn,b,type = "raw")*3470000
hist(b$pre)

ggplot(b,aes(pre))+geom_histogram()

median(b$pre)
b$pre[b$pre<000000]=900000

write.csv(b$pre, file = "nn.csv",row.names = F)
