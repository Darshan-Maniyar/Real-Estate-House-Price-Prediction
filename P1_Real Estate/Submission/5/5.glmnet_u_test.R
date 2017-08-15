str(house_test)
house_test$Price=NULL


b_train=house_test


str(b_train)




##==============LM===================
# 
# fit=qlm(data = b_train,V_dependent = Price)
# summary(fit)
# 
# 
# b_train$Method_S=NULL
# b_train$CouncilArea_Boroondara=NULL


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


x.b_train=as.matrix(b_train)
x.b_train=x.b_train[,-4]
x.b_train=Matrix(x.b_train,sparse = T)
dim(x.b_train)
# x.b_train=cbind(x.b_train,x.b_train[1:3]^2,x.b_train[1:3]^3,x.b_train[1:3]^4)

yhat0 <- predict(fit_u7, s=fit_u7$lambda.1se, newx=x.b_train)
hist(exp(yhat0))

write.csv(data.frame( b_train$sr.no,exp(yhat0)), file = "5LM_u.csv",row.names = F)
