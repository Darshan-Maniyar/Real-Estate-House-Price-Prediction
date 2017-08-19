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
rpivotTable(house, rows="Bedroom2", col="Bedroom2", aggregatorName="Average", 
            vals="Price", rendererName="Treemap")
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

# Rooms	Totals
# 1	    428,781.34
# 2	    745,289.96
# 3	  1,095,378.70
# 4	  1,558,278.84
# 5	  2,020,829.47
# 6	  2,164,300.00
# 7	  1,492,000.00
# 8	  1,868,500.00
# 9	  1,380,000.00
# Tot	1,069,723.35

m=median(house$Price[house$Rooms==1])

a=m-(m*0.90)
b=m+(m*0.90)
house$Price[house$Rooms==1 &  house$Price< (a) ]=m
house$Price[house$Rooms==1 &  house$Price> (b) ]=m

m=median(house$Price[house$Rooms==2])
a=m-(m*0.90)
b=m+(m*0.90)
house$Price[house$Rooms==2 &  house$Price< (a) ]=m
house$Price[house$Rooms==2 &  house$Price> (b) ]=m

m=median(house$Price[house$Rooms==3])
a=m-(m*0.90)
b=m+(m*0.90)
house$Price[house$Rooms==3 &  house$Price< (a) ]=m
house$Price[house$Rooms==3&  house$Price> (b) ]=m

m=median(house$Price[house$Rooms==4])
a=m-(m*0.90)
b=m+(m*0.90)
house$Price[house$Rooms==4 &  house$Price< (a) ]=m
house$Price[house$Rooms==4 &  house$Price> (b) ]=m

m=median(house$Price[house$Rooms==5])
a=m-(m*0.90)
b=m+(m*0.90)
house$Price[house$Rooms==5 &  house$Price< (a) ]=m
house$Price[house$Rooms==5 &  house$Price> (b) ]=m

m=median(house$Price[house$Rooms==6])
a=m-(m*0.90)
b=m+(m*0.90)
house$Price[house$Rooms==6 &  house$Price< (a) ]=m
house$Price[house$Rooms==6 &  house$Price> (b) ]=m

hist(house$Price)
b=house
str(b)
b=b[-c(2,7,9,15)]
colSums(is.na(b))


b=dummy(b,c("Suburb","Type","Method","CouncilArea"),limit = 200,rm_original = T)


# dmy=dummyVars("~.",b)
# b=data.frame(predict(dmy,b))
a=which(names(b) %in%"Price")
b[-a]=scale(b[-a])
set.seed(2)
a1=sample(1:nrow(b), nrow(b)*0.70)
b_train=b[a1,]
b_test=b[-a1,]
dim(b_test)



b1=as.matrix(b_train[a])
b2=as.matrix(b_train[-a])

b3=as.matrix(b_test[a])
b4=as.matrix(b_test[-a])


xgbd=xgb.DMatrix(data=b2,label=b1)
xgbdtest=xgb.DMatrix(data=b4,label=b3)



param <- list("eta" =0.05,
              
              "max.depth"=9,
              # "min_child_weight"=0.9,
              "gamma"=0,
              # "lambda"=10,"lambda_bias"=0,"alpha"=2,
              "nthread"=32,
              "subsample"=0.9,
              # "colsample_bytree"=0.5,
              # "scale_pos_weight"=1,
              "metric" = "rmse"
              
              # "objective"="binary:logistic"
              # "booster"="dart",
              # "sample_type"="weighted",
              # "normalize_type"="forest"
) 

set.seed(2)
bst.cv = xgb.cv(param=param,  xgbd ,nrounds=3000 ,early_stopping_rounds=50,nfold = 4,seed=27)#,"maximize"=F,niter=10,  ,niter=500, nrounds = 50,colsample_bytree =0.9,gamma =100,min_child_weight=10,subsample =0.9)#)#prediction=TRUE, verbose=FALSE,
bst.cv$evaluation_log
bst.cv$best_iteration
max.auc.idx = which.max(bst.cv$evaluation_log[,test_auc_mean]) 
max.auc.idx 
bst.cv$evaluation_log[max.auc.idx,]
summary(bst.cv)
plot(bst.cv$evaluation_log[,iter],bst.cv$evaluation_log[,test_rmse_mean])

set.seed(2)
bst <- xgboost(param=param,xgbd ,nrounds=214 ,nfold = 4,seed=27,verbose = 0)#,niter=299, early_stopping_rounds=50,min_child_weight=10,subsample =0.9)#


names <- dimnames(b2)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
xgb.plot.importance(importance_matrix[1:25])


btr=b_train

set.seed(2)
btr$pred=predict(bst,xgbd,ntreelimit = 214)


res=btr$Price-btr$pred
plot(btr$Distance,btr$pred)
a=data.frame(Price=btr$Price,pred=btr$pred,res=res)
ggplot(a,aes(Price,res))+geom_point()+geom_smooth()
ggplot(a,aes(Price,pred))+geom_point()+geom_smooth()


rmse_test=sqrt(mean((res)^2))#[res<1000000]
rmse_test
plot(log(bst.cv$evaluation_log[,4]),type = "l")

##============== test on train_test set=============
dim(bt)

bt=b_test
bt=b_test[1:1000,]
set.seed(2)
bt$pred=predict(bst,xgbdtest,ntreelimit = 214)


res=bt$Price-bt$pred
plot(bt$Distance,bt$pred)
a=data.frame(Price=bt$Price,pred=bt$pred,res=res)
ggplot(a,aes(Price,res))+geom_point()+geom_smooth()
ggplot(a,aes(Price,pred))+geom_point()+geom_smooth()


rmse_test=sqrt(mean((res)^2))#[res<1000000]
rmse_test
plot(log(bst.cv$evaluation_log[,4]),type = "l")


x=data.frame(m= 1:50)
a=1:100

set.seed(2)
z=apply(x,1,function(y){
  set.seed(y)  
  a=sample(1:nrow(b_test),nrow(b_test)*0.1)
  bt1=b_test[a,]
  a=which(names(bt1) %in%"Price")
  b5=as.matrix(bt1[a])
  b6=as.matrix(bt1[-a])
  
  xgbdtest1=xgb.DMatrix(data=b6,label=b5)
  
  bt1$pred=predict(bst,xgbdtest1,ntreelimit =214 )#as.matrix(h1_test[2:425])
  res=bt1$Price-bt1$pred
  
  
  rmse_test=sqrt(mean((res)^2))
  return(c(rmse_test))#,a2["Matrix"]
})
summary(z)

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
