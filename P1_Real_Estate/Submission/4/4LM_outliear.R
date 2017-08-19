library(ggplot2)
library(dplyr)
library(car)
library(plyr)
library(quickregression)
library(tidyr)
library(dumm)
library(xgboost)
library(rpivotTable)

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
house$srno=1:nrow(house)
a=sapply (house$Address, function(x) tail(strsplit(x,split=" ")[[1]],1))
# View(a)



a=as.matrix(a)
table(a)
length(house$road %in% names(sort(table(a),decreasing = T))[1:8])
colSums(is.na(house))
house$road=(a)
house$road=as.character(house$road)

sum(table(house$road[! house$road %in% names(sort(table(a),decreasing = T))[1:15]]))
sort(table(house$road[! house$road %in% names(sort(table(a),decreasing = T))[1:15]]),decreasing = T)
house$road[! house$road %in% names(sort(table(a),decreasing = T))[1:15]]="others"
sort(table(house$road),decreasing = T)

house$shouse= grepl("/", house$Address)
house$shouse=ifelse(house$shouse=="TRUE",1,0)
# house$Suburb=NULL
house$Address=NULL
#house$SellerG=NULL


table(house$Bedroom2,house$Type)

house[is.na(house$Bedroom2) & house$Type=="h",c("Bedroom2")]=3
house[is.na(house$Bedroom2) & house$Type=="t",c("Bedroom2")]=3
house[is.na(house$Bedroom2) & house$Type=="u",c("Bedroom2")]=2

house[(house$Bedroom2)>6 & house$Type=="h",c("Bedroom2")]=6

# house[house$Bedroom2>5,c("Bedroom2")]=3


# xtabs(Rooms~Bathroom+Type+Bedroom2,data=house)
table(house$Bathroom,house$Type)


house[is.na(house$Bathroom) & house$Type=="h",c("Bathroom")]=1
house[is.na(house$Bathroom) & house$Type=="t",c("Bathroom")]=2
house[is.na(house$Bathroom) & house$Type=="u",c("Bathroom")]=1

house[(house$Bathroom)>5 & house$Type=="h",c("Bathroom")]=5

# house[house$Bathroom>4,c("Bathroom")]=1



table(house$Car,house$Type)
house[is.na(house$Car) & house$Type=="h",c("Car")]=2
house[is.na(house$Car) & house$Type=="t",c("Car")]=2
house[is.na(house$Car) & house$Type=="u",c("Car")]=1

house[(house$Car)>5 & house$Type=="h",c("Car")]=5
house[(house$Car)>5 & house$Type=="u",c("Car")]=4

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
# a=house %>% na.omit() %>% select(Suburb,Landsize) %>% group_by(Suburb) %>%  dplyr::summarise(mea=mean(Landsize))

# View(a)
# a=a[order(a$mea,decreasing = T),]
# a$YearBuilt1=c((rep(1:25,each=5)),rep(26,10))
# hist(a$mea)
# 
# house=merge(a[c(1,3)],house,by="YearBuilt")
# house$YearBuilt=NULL

# house[is.na(house$Landsize),c("Landsize","Suburb")]
# house=merge(a[c(1,2)],house,by="Suburb")
# str(house)
# sum(is.na(house1$mea))
# house$Landsize[is.na(house$Landsize)]=house$mea[is.na(house$Landsize)]

# house$Landsize=house1$Landsize

hist(house$Landsize)
house$Landsize[is.na(house$Landsize)]=median(house$Landsize,na.rm = T)
house$Landsize=log(house$Landsize+1)
# hist((house$Landsize)^0.5)
# boxplot((house$Landsize))

##===BuildingArea NA removal========



hist(house$BuildingArea)
house$BuildingArea[is.na(house$BuildingArea)]=median(house$BuildingArea,na.rm = T)
house$BuildingArea=log(house$BuildingArea+1)
# house[house$BuildingArea>295,"BuildingArea"]=median(house$BuildingArea,na.rm = T)
# hist((house$BuildingArea)^0.5)

# boxplot(house$BuildingArea)

##===YearBuilt NA removal========

sort(table(house$YearBuilt),decreasing = T)
table(house$YearBuilt,house$Type)
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

# View(house[house$BuildingArea>house$Landsize & house$Landsize!=0,])

# house=house %>% mutate(tot_room = Rooms+Bedroom2+Bathroom+Car) %>% select(-Rooms,-Bedroom2,-Bathroom,-Car)


# house$Landsize[grep("/",house$Address)] 

house=house[house$Price<5000000,]

a=house %>% select(Suburb,Price) %>% group_by(Suburb) %>%  dplyr::summarise(mea=mean(Price))


# View(a)
a=a[order(a$mea,decreasing = T),]
a$Suburb1=c((rep(1:20,each=7)),21,21)
hist(a$mea)

house=merge(a[c(1,3)],house,by="Suburb")
house$Suburb=NULL

# a=house[house$Price>0,] %>% select(Postcode,Price) %>% group_by(Postcode) %>%  dplyr::summarise(mea=mean(Price))
# 
# # View(a)
# a=a[order(a$mea,decreasing = T),]
# a$Postcode1=c((rep(1:23,each=4)),24,24)
# hist(a$mea)
# 
# house=merge(a[c(1,3)],house,by="Postcode")
# house$Postcode=NULL

# a=house %>% select(SellerG,Price) %>% group_by(SellerG) %>%  dplyr::summarise(mea=mean(Price))
# 
# # View(a)
# a=a[order(a$mea,decreasing = T),]
# # a$SellerG1=c((rep(1:20,each=9)),21,21)
# a$SellerG1=c((rep(1:20,each=9)),rep(21,8))
# hist(a$mea)
# 
# house=merge(a[c(1,3)],house,by="SellerG")
# house$SellerG=NULL

a=house %>% select(YearBuilt,Price) %>% group_by(YearBuilt) %>%  dplyr::summarise(mea=mean(Price))

# View(a)
a=a[order(a$mea,decreasing = T),]
a$YearBuilt1=c((rep(1:25,each=5)),rep(26,10))
hist(a$mea)

house=merge(a[c(1,3)],house,by="YearBuilt")
house$YearBuilt=NULL

str(house)
hist(house$Price)
b=house
str(b)
# b=b[-c(5,9)]
colSums(is.na(b))
# b$CouncilArea=NULL
b$SellerG=NULL
# b$YearBuilt=NULL
# b$Postcode=NULL

b=dummy(b,c("YearBuilt1","Postcode","Suburb1","CouncilArea","Rooms","Type","Method","Bedroom2","Bathroom","Car","road"),limit = 500,rm_original = T)
# b=dummy(b,c("YearBuilt","Suburb","Type","Method","Postcode","road"),limit = 500,rm_original = T)

dim(b)

house_train=b[b$is_train==1,]
house_train$is_train=NULL

house_test=b[b$is_train==0,]
house_test$is_train=NULL


b=house_train
set.seed(2)
a1=sample(1:nrow(b), nrow(b)*1)
b_train=b[a1,]
b_test=b[-a1,]
dim(b_test)
str(b_train)




##==============LM===================



b_train$Distance1=b_train$Distance^2
b_train$Distance2=b_train$Distance^3
b_train$Distance3=b_train$Distance^4
b_train$Distance4=b_train$Distance^5
b_train$Distance5=b_train$Distance^6


b_train$Landsize1=b_train$Landsize^2
b_train$Landsize2=b_train$Landsize^3
b_train$Landsize3=b_train$Landsize^4
b_train$Landsize4=b_train$Landsize^5
b_train$Landsize5=b_train$Landsize^6

b_train$BuildingArea1=b_train$BuildingArea^2
b_train$BuildingArea2=b_train$BuildingArea^3
b_train$BuildingArea3=b_train$BuildingArea^4
b_train$BuildingArea4=b_train$BuildingArea^5
b_train$BuildingArea5=b_train$BuildingArea^6

library(glmnet)


x.b_train=as.matrix(b_train[-4])
y.b_train=as.matrix(b_train[4])



dim(as.data.frame(x.b_train))


library(Matrix)
x.b_train=Matrix(x.b_train,sparse = T)
# y.b_train=Matrix(y.b_train,sparse = T)
y.b_train=log(y.b_train+1)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x.b_train, y.b_train, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

fit0$cvm


yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.b_train)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.b_train)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.b_train)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.b_train)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.b_train)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.b_train)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.b_train)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.b_train)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.b_train)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.b_train)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.b_train)

mse0 <- sqrt(mean((y.b_train - yhat0)^2))
mse1 <- sqrt(mean((y.b_train - yhat1)^2))
mse2 <- sqrt(mean((y.b_train - yhat2)^2))
mse3 <- sqrt(mean((y.b_train - yhat3)^2))
mse4 <- sqrt(mean((y.b_train - yhat4)^2))
mse5 <- sqrt(mean((y.b_train - yhat5)^2))
mse6 <- sqrt(mean((y.b_train - yhat6)^2))
mse7 <- sqrt(mean((y.b_train - yhat7)^2))
mse8 <- sqrt(mean((y.b_train - yhat8)^2))
mse9 <- sqrt(mean((y.b_train - yhat9)^2))
mse10 <- sqrt(mean((y.b_train - yhat10)^2))

mse=c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10)
mse

210000/313078.8

colSums(is.na(yhat1))
dim((y.b_train))

plot(exp(c(yhat8)) ~ exp(c(y.b_train)))
sqrt(mean((exp(c(yhat8)) - exp(c(y.b_train)))^2))
plot(fit0, xvar="lambda")
hist(yhat0)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)



hist(mse)

sds <- apply(x.b_train, 2, sd)
cs <- as.matrix(coef(fit8, s = "lambda.min"))
std_coefs <- cs[-1, 1] / sds

sort(std_coefs,decreasing = T)

coef(fit8, s = "lambda.min")

fit.lasso <- glmnet(x.b_train, y.b_train, family="gaussian", alpha=0.8)
fit.lasso
plot(fit.lasso$jerr)
plot(fit.lasso,xvar="norm")
plot(fit.lasso,xvar="dev")


yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x.test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x.test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x.test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x.test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x.test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x.test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x.test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x.test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x.test)
y.test=log(y.test)

mse0 <- sqrt(mean((y.test - yhat0)^2))
mse1 <- sqrt(mean((y.test - yhat1)^2))
mse2 <- sqrt(mean((y.test - yhat2)^2))
mse3 <- sqrt(mean((y.test - yhat3)^2))
mse4 <- sqrt(mean((y.test - yhat4)^2))
mse5 <- sqrt(mean((y.test - yhat5)^2))
mse6 <- sqrt(mean((y.test - yhat6)^2))
mse7 <- sqrt(mean((y.test - yhat7)^2))
mse8 <- sqrt(mean((y.test - yhat8)^2))
mse9 <- sqrt(mean((y.test - yhat9)^2))
mse10 <- sqrt(mean((y.test - yhat10)^2))

mse=c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10)

plot(exp(c(yhat10)) ~ exp(c(y.test)))
sqrt(mean((exp(c(yhat10)) - exp(c(y.test)))^2))

small.lambda.index <- which(fit10$lambda == fit10$lambda.min)
small.lambda.betas <- fit10$glmnet.fit$beta[, small.lambda.index]


210000/322882

plot(fit6)
b_train$Price

sqrt(2e11)
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

###=====Corr==========

str(house)
house1=dumm::dummy( data=house,vnames = c("Type","Method","CouncilArea"),limit = 50 )

house1=house1 %>% na.omit()
cor(house1)

library(corrplot)
windows()
corrplot(cor(house1))

table(house$YearBuilt)
hist(house$YearBuilt)

