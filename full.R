#reading files
train <- read.csv("train.csv", stringsAsFactors = T)
str(train)
test <- read.csv("test.csv", stringsAsFactors = T)
View(test)
summary(train) #var identification




test$Footfall <- 0
testtrainold <-rbind(train,test)

#missing values 
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(testtrainold,2,pMiss)
#library(dplyr)
#missing <- testtrainold %>% group_by(Park_ID) %>% summarise(apply(MARGIN = 2,FUN = pMiss(testtrain)) )



library(mice)
missingpattern <- md.pattern(testtrainold)
missingpattern <- as.data.frame(missingpattern)

tempData <- mice(testtrainold[c(4:17)],m=2,maxit=2,meth='pmm',seed=500)
summary(tempData)
completedData <- complete(tempData,1)

testtrainold <- cbind(testtrainold[c(1,2,3,18)],completedData)

rm(completedData,tempData)
write.csv(testtrainold, "testtrainoldwithoutfeatures.csv")


##divide to test, train 
test_new <- subset(testtrainold,testtrainold$Footfall == 0)
train_new <-  subset(testtrainold,testtrainold$Footfall > 0)
#trial model rpart

library(rpart)
library(rpart.plot)
traindt <- train_new[c(2:18)]
testdt <- test_new[c(2:17)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt ,method = "anova",control = rpart.control(minbucket = 50))
prp(fitdt)
summary(fitdt)
#Predict Output 
predicteddt = predict(fitdt,testdt)
test_new$Footfall <- predicteddt
submission4 <- test_new[c("ID","Footfall")]
write.csv(submission4 , "submission4.csv")
##correlation
#now you can see correlatons if you wish to


#feature

testtrainold$date <- as.Date(testtrainold$Date, "%d-%m-%Y")
testtrainold <- testtrainold[-3] #Date

testtrainold$Range_Breeze_Speed <- testtrainold$Max_Breeze_Speed - testtrainold$Min_Breeze_Speed
testtrainold$Range_Atm_Pr <- testtrainold$Max_Atmospheric_Pressure- testtrainold$Min_Atmospheric_Pressure
testtrainold$Range_Ambt_Pollution <- testtrainold$Max_Ambient_Pollution- testtrainold$Min_Ambient_Pollution
testtrainold$Range_Moisture <- testtrainold$Max_Moisture_In_Park- testtrainold$Min_Moisture_In_Park
testtrainold$Park_ID <- as.factor(testtrainold$Park_ID)
install.packages("lubridate")
library(lubridate)
testtrainold$month <- month(as.POSIXlt(testtrainold$date))
testtrainold$Temperature <- ((testtrainold$Average_Moisture_In_Park)^.25)*10

testtrainold$week <- week(testtrainold$date)
testtrainold$weekday <- wday(as.POSIXlt(testtrainold$date))
testtrainold$Direction_Of_Wind <- .bincode(testtrainold$Direction_Of_Wind , c(1,45,90,135,180,225,270,315,360) , right = T , include.lowest = T)
testtrainold$days <- julian(as.POSIXlt(testtrainold$date))


library(dplyr)
testtrainold =testtrainold %>% mutate(avg_breeze_speed_quantile = ntile(testtrainold$Average_Breeze_Speed, 10))
testtrainold =testtrainold %>% mutate(Average_Atmospheric_Pressure_quantile = ntile(testtrainold$Average_Atmospheric_Pressure, 10))
testtrainold =testtrainold %>% mutate(Min_Ambient_Pollution_quant = ntile(testtrainold$Min_Ambient_Pollution, 10))
testtrainold =testtrainold %>% mutate(Max_Ambient_Pollution_quant = ntile(testtrainold$Max_Ambient_Pollution, 10))
testtrainold =testtrainold %>% mutate(Average_Moisture_In_Park_quant = ntile(testtrainold$Average_Moisture_In_Park, 10))
testtrainold =testtrainold %>% mutate(Min_Moisture_In_Park_quant = ntile(testtrainold$Min_Moisture_In_Park, 10))
testtrainold =testtrainold %>% mutate(Max_Moisture_In_Park_quant = ntile(testtrainold$Max_Moisture_In_Park, 10))

testtrainold$quartilerainfall <- ifelse(testtrainold$Var1 == 0,0,ifelse((testtrainold$Var1 < .83) & (testtrainold$Var1 > 0)  ,1,ifelse((testtrainold$Var1 < 21.58)&(testtrainold$Var1 >= 0.83), 2,3)))





write.csv(testtrainold,"testtrainoldbeforetransformation.csv")


###transfrmations
testtrainold$Var1 <- log(testtrainold$Var1 + (1+ (testtrainold$Var1)^2)^.5)
testtrainold$Max_Moisture_In_Park <- (testtrainold$Max_Moisture_In_Park)^4
testtrainold$Max_Ambient_Pollution <- testtrainold$Max_Ambient_Pollution^3
testtrainold$Max_Breeze_Speed <- log(testtrainold$Max_Breeze_Speed)

####missing in dir of wind
testtrainold$Direction_Of_Wind[is.na(testtrainold$Direction_Of_Wind)] = mean(testtrainold$Direction_Of_Wind,na.rm = T)

testtrain$Direction_Of_Wind[is.na(testtrain$Direction_Of_Wind)] = mean(testtrain$Direction_Of_Wind, na.rm = T)


######complete rearrangement of columns
testtrain <- testtrainold[,c(1:17,19:31,18)]


#####factorconversion
testtrain_factor = testtrain[,-c(4,5:16,18,19,31)]
testtrain_factor =  sapply(testtrain_factor, as.factor)
testtrain_factor = as.data.frame(testtrain_factor)
str(testtrain_factor)
testtrain = cbind(testtrain_factor,testtrain[,c(4,5:16,18,19,31)]) 
str(testtrain)

###remove unnecessary

testtrain <- testtrain[-c(6,41)]#var1.y and yrr removed
testtrain <- testtrain[-c(4)]    #rainfall 0 or1




#######subset
test_new <- subset(testtrain,testtrain$Footfall == 0)
train_new <- subset(testtrain,testtrain$Footfall > 0)

##########################2 sets based on months 


train_new$month <- as.numeric(train_new$month)
test_new$month <- as.numeric(test_new$month)
testtrain$month  <- as.numeric(testtrain$month)
testtrainold$date <- as.Date(testtrainold$date)
testtrain$date <- as.Date(testtrain$date)
train_new1 <- subset(train_new,(train_new$month >3) & (train_new$month < 10) )
train_new2 <- subset(train_new,(train_new$month  < 4) | (train_new$month > 9) )

test_new1 <- subset(test_new,(test_new$month >3) & (test_new$month < 10) )
test_new2 <- subset(test_new,(test_new$month  < 4) | (test_new$month > 9) )

testtrain1 <- subset(testtrain,(testtrain$month >3) & (testtrain$month < 10) )
testtrain2 <- subset(testtrain,(testtrain$month  < 4) | (testtrain$month > 9) )








##########################
#load library
install.packages("dummies")
library(dummies)

####numerical variables
#1


pca.train <- testtrain1[testtrain1$Footfall > 0,]
pca.train1 <- testtrain1[testtrain1$Footfall > 0,]
pca.test <- testtrain1[testtrain1$Footfall == 0,]
pca.test1 <- testtrain1[testtrain1$Footfall == 0,]
pca.train <- pca.train[-c(3,4,31)]
pca.test <- pca.test[-c(3,4,31)]
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

summary(prin_comp)
prin_comp$rotation
prin_comp$x


std_dev <- prin_comp$sdev
pr_var <- std_dev^2

pr_var[1:10]
####prop var explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]


plot(prop_varex,type = "b")
plot(cumsum(prop_varex),type = "b")


##now 
train.data <- data.frame(Footfall = pca.train1$Footfall, prin_comp$x)
train.data <- train.data[,1:13]

library(randomForest)

rf.model <- randomForest(Footfall ~. , data = train.data,method = "anova"  , ntree = 100 , nodesize= 25)


##transform test
test.data <- predict(prin_comp ,newdata = pca.test)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:13]

#make prediction
rpart.prediction <- predict(rf.model,test.data)
predictionpca <- as.data.frame(rpart.prediction)

pca.test1$Footfall <- predictionpca$rpart.prediction
pca.test1 <- pca.test1[c("ID","Footfall")]



#2


pca.train <- testtrain2[testtrain2$Footfall > 0,]
pca.train1 <- testtrain2[testtrain2$Footfall > 0,]
pca.test <- testtrain2[testtrain2$Footfall == 0,]
pca.test2 <- testtrain2[testtrain2$Footfall == 0,]
pca.train <- pca.train[-c(3,4,31)]
pca.test <- pca.test[-c(3,4,31)]
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

summary(prin_comp)
prin_comp$rotation
prin_comp$x


std_dev <- prin_comp$sdev
pr_var <- std_dev^2

pr_var[1:10]
####prop var explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]


plot(prop_varex,type = "b")
plot(cumsum(prop_varex),type = "b")


##now 
train.data <- data.frame(Footfall = pca.train1$Footfall, prin_comp$x)
train.data <- train.data[,1:12]

library(randomForest)

rf.model2 <- randomForest(Footfall ~. , data = train.data,method = "anova" ,mtry = 4  )
summary(rf.model2)

##transform test
test.data <- predict(prin_comp ,newdata = pca.test)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:12]

#make prediction
rpart.prediction <- predict(rf.model2,test.data)
predictionpca <- as.data.frame(rpart.prediction)

pca.test2$Footfall <- predictionpca$rpart.prediction
pca.test2 <- pca.test2[c("ID","Footfall")]




pca.test <- rbind(pca.test1,pca.test2)
write.csv(pca.test,"submissionpca.csv")




#####nancy####
##########extractind  data location wise #####

exp = train_new[,-c(2,3)]
exp$weekday = wday(exp$date)
exp = exp[,-c(15)]
colnames(exp)[16:19] = c("loc1","loc2","loc3","loc4")
loc1 = subset(exp,exp$loc1==1)
loc2 = subset(exp,exp$loc2==1)
loc3 = subset(exp,exp$loc3==1)
loc4 = subset(exp,exp$loc4==1)
loc1= loc1[,-c(16,17,18,19)]
loc2= loc2[,-c(16,17,18,19)]
loc3= loc3[,-c(16,17,18,19)]
loc4= loc4[,-c(16,17,18,19)]




test_exp = test_new[,-c(2,3)]
test_exp$weekday = wday(test_exp$date)
test_exp = test_exp[,-c(15)]
colnames(test_exp)[16:19] = c("loc1","loc2","loc3","loc4")
test_loc1 = subset(test_exp,test_exp$loc1==1)
test_loc2 = subset(test_exp,test_exp$loc2==1)
test_loc3 = subset(test_exp,test_exp$loc3==1)
test_loc4 = subset(test_exp,test_exp$loc4==1)
test_loc1= test_loc1[,-c(16,17,18,19)]
test_loc2= test_loc2[,-c(16,17,18,19)]
test_loc3= test_loc3[,-c(16,17,18,19)]
test_loc4= test_loc4[,-c(16,17,18,19)]


######location1#####
fitControl <- trainControl(method = "cv",
                           number = 10)

gbmGrid = expand.grid(interaction.depth = 2,
                      n.trees = 500,
                      shrinkage = 0.1,
                      n.minobsinnode = 10)
set.seed(825)
fit1 <- train(Footfall~.-ID,data=loc1,
              method = "gbm",
              trControl = fitControl,
              verbose = FALSE,
              tuneGrid = gbmGrid )
summary(fit1)

predicted1 = predict(fit1,test_loc1)
predicted
#######LOCATION2#####
fit2 <- train(Footfall~.-ID,data=loc2,
              method = "gbm",
              trControl = fitControl,
              verbose = FALSE,
              tuneGrid = gbmGrid)

predicted2 = predict(fit2,test_loc2)

####location3 ###
fit3 <- train(Footfall~.-ID,data=loc3,
              method = "gbm",
              trControl = fitControl,
              verbose = FALSE,
              tuneGrid = gbmGrid)

predicted3 = predict(fit3,test_loc3)

#####location4####
fit4 <- train(Footfall~.-ID,data=loc4,
              method = "gbm",
              trControl = fitControl,
              verbose = FALSE,
              tuneGrid = gbmGrid)

predicted4 = predict(fit4,test_loc4)
pred1 = as.data.frame(predicted1)
pred2 = as.data.frame(predicted2)
pred3 = as.data.frame(predicted3)
pred4 = as.data.frame(predicted4)

#######combining  predictedfootfall with test_new ########

test_loc1$Footfall = 0
test_loc2$Footfall = 0
test_loc3$Footfall = 0
test_loc4$Footfall = 0
test_loc1$Footfall = pred1$predicted1
test_loc2$Footfall = pred2$predicted2
test_loc3$Footfall = pred3$predicted3
test_loc4$Footfall = pred4$predicted4

test_final =rbind(test_loc1,test_loc2,test_loc3,test_loc4)

test_required = test_final[,c(1,21)]







#Lasso 

install.packages("lars")
# load the package
library(lars)

x <- as.matrix(train_new1[,c(5,17:30)])
y <- as.matrix(train_new1[31])
# fit model
fit <- lars(x, y, type="lasso")

# summarize the fit
summary(fit)
# select a step with a minimum error
best_step <- fit$df[which.min(fit$RSS)]
# make predictions
predictions <- predict(fit, x, s=best_step, type="fit")$fit
# summarize accuracy
rmse <- (mean((y - predictions)^2))^0.5
print(rmse)

#predictions by lasso

testx <- as.matrix(test_new1[,c(5,17:30)])
predictiontestx <- predict(fit, testx, s=best_step, type="fit")$fit
test_new1$Footfall <- predictiontestx
submissionlasso1 <- test_new1[c("ID","Footfall")]


library(lars)

x <- as.matrix(train_new2[,c(5,17:30)])
y <- as.matrix(train_new2[31])
# fit model
fit <- lars(x, y, type="lasso")

# summarize the fit
summary(fit)
# select a step with a minimum error
best_step <- fit$df[which.min(fit$RSS)]
# make predictions
predictions <- predict(fit, x, s=best_step, type="fit")$fit
# summarize accuracy
rmse <- (mean((y - predictions)^2))^0.5
print(rmse)

#predictions by lasso

testx <- as.matrix(test_new2[,c(5,17:30)])
predictiontestx <- predict(fit, testx, s=best_step, type="fit")$fit
test_new2$Footfall <- predictiontestx
submissionlasso2 <- test_new2[c("ID","Footfall")]




submissionlasso <- rbind(submissionlasso1,submissionlasso2)
write.csv(submissionlasso , "submissionlasso.csv")





#DT
library(rpart)
library(rpart.plot)
traindt <- train_new1[c(2:31)]
testdt <- test_new1[c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt ,method = "anova",control = rpart.control(minbucket = 50 ))
prp(fitdt)
summary(fitdt)
#Predict Output 
predicteddt = predict(fitdt,testdt)
test_new1$Footfall <- predicteddt
submissiondt1 <- test_new1[c("ID","Footfall")]

traindt <- train_new2[c(2:31)]
testdt <- test_new2[c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt ,method = "anova",control = rpart.control(minbucket = 50))
prp(fitdt)
summary(fitdt)
#Predict Output 
predicteddt = predict(fitdt,testdt)
test_new2$Footfall <- predicteddt
submissiondt2 <- test_new2[c("ID","Footfall")]



submissiondt <- rbind(submissiondt1,submissiondt2)
write.csv(submissiondt, "submissiondt.csv")




traindt <- train_new1[c(2:31)]
testdt <- test_new1[c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt ,method = "anova",control = rpart.control(minbucket = 50 ))
prp(fitdt)
summary(fitdt)
#Predict Output 
predicteddt = predict(fitdt,testdt)
test_new1$Footfall <- predicteddt
submissiondt1 <- test_new1[c("ID","Footfall")]

traindt <- train_new2[c(2:31)]
testdt <- test_new2[c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt ,method = "anova",control = rpart.control(minbucket = 50))
prp(fitdt)
summary(fitdt)
#Predict Output 
predicteddt = predict(fitdt,testdt)
test_new2$Footfall <- predicteddt
submissiondt2 <- test_new2[c("ID","Footfall")]



submissiondt <- rbind(submissiondt1,submissiondt2)
write.csv(submissiondt, "submissiondt.csv")


######DT individually ######
exp1 <- subset(train_new1,train_new1$Location_Type == 1)
exp2 <- subset(train_new1,train_new1$Location_Type == 2)
exp3 <- subset(train_new1,train_new1$Location_Type == 3)
exp4 <- subset(train_new1,train_new1$Location_Type == 4)
testexp1 <- subset(test_new1,test_new1$Location_Type == 1)
testexp2 <- subset(test_new1,test_new1$Location_Type == 2)
testexp3 <- subset(test_new1,test_new1$Location_Type == 3)
testexp4 <- subset(test_new1,test_new1$Location_Type == 4)
library(rpart)
library(rpart.plot)
traindt1 <- exp1[,c(2:31)]
testdt1 <- testexp1[,c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt1 ,method = "anova",control = rpart.control(minbucket = 60))
prp(fitdt)
summary(fitdt)
###pruning and cv
cpdt = fitdt$cptable[which.min(fitdt$cptable[,"xerror"]),"CP"]
ptree<- prune(fitdt, cp=cpdt)

#Predict Output 
predicteddt1 = predict(ptree,testdt1)
testexp1$Footfall <- predicteddt1

#2
traindt2 <- exp2[,c(2:31)]
testdt2 <- testexp2[,c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt2 ,method = "anova",control = rpart.control(minbucket = 60))
prp(fitdt)
summary(fitdt)
###pruning and cv
cpdt = fitdt$cptable[which.min(fitdt$cptable[,"xerror"]),"CP"]
ptree<- prune(fitdt, cp= cpdt)
#Predict Output 
predicteddt2 = predict(ptree,testdt2)
testexp2$Footfall <- predicteddt2

#3
traindt3 <- exp3[,c(2:31)]
testdt3 <- testexp3[,c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt3 ,method = "anova",control = rpart.control(minbucket = 60))
prp(fitdt)
summary(fitdt)
###pruning and cv
cpdt = fitdt$cptable[which.min(fitdt$cptable[,"xerror"]),"CP"]
ptree<- prune(fitdt, cp= cpdt)
#Predict Output 
predicteddt3 = predict(ptree,testdt3)
testexp3$Footfall <- predicteddt3

#4
traindt4 <- exp4[,c(2:31)]
testdt4 <- testexp4[,c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt4 ,method = "anova",control = rpart.control(minbucket = 60))
prp(fitdt)
summary(fitdt)
###pruning and cv
cpdt = fitdt$cptable[which.min(fitdt$cptable[,"xerror"]),"CP"]
ptree<- prune(fitdt, cp= cpdt)
#Predict Output 
predicteddt4 = predict(ptree,testdt4)
testexp4$Footfall <- predicteddt4
testdtunique1 <- rbind(testexp1,testexp2,testexp3,testexp4)


######DT individually ######
exp1 <- subset(train_new2,train_new2$Location_Type == 1)
exp2 <- subset(train_new2,train_new2$Location_Type == 2)
exp3 <- subset(train_new2,train_new2$Location_Type == 3)
exp4 <- subset(train_new2,train_new2$Location_Type == 4)
testexp1 <- subset(test_new2,test_new2$Location_Type == 1)
testexp2 <- subset(test_new2,test_new2$Location_Type == 2)
testexp3 <- subset(test_new2,test_new2$Location_Type == 3)
testexp4 <- subset(test_new2,test_new2$Location_Type == 4)
library(rpart)
library(rpart.plot)
traindt1 <- exp1[,c(2:31)]
testdt1 <- testexp1[,c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt1 ,method = "anova",control = rpart.control(minbucket = 60))
prp(fitdt)
summary(fitdt)
###pruning and cv
cpdt = fitdt$cptable[which.min(fitdt$cptable[,"xerror"]),"CP"]
ptree<- prune(fitdt, cp= cpdt)

#Predict Output 
predicteddt1 = predict(ptree,testdt1)
testexp1$Footfall <- predicteddt1

#2
traindt2 <- exp2[,c(2:31)]
testdt2 <- testexp2[,c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt2 ,method = "anova",control = rpart.control(minbucket = 60))
prp(fitdt)
summary(fitdt)
###pruning and cv
cpdt = fitdt$cptable[which.min(fitdt$cptable[,"xerror"]),"CP"]
ptree<- prune(fitdt, cp= cpdt)
#Predict Output 
predicteddt2 = predict(ptree,testdt2)
testexp2$Footfall <- predicteddt2

#3
traindt3 <- exp3[,c(2:31)]
testdt3 <- testexp3[,c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt3 ,method = "anova",control = rpart.control(minbucket = 60))
prp(fitdt)
summary(fitdt)
###pruning and cv
cpdt = fitdt$cptable[which.min(fitdt$cptable[,"xerror"]),"CP"]
ptree<- prune(fitdt, cp= cpdt)
#Predict Output 
predicteddt3 = predict(ptree,testdt3)
testexp3$Footfall <- predicteddt3

#4
traindt4 <- exp4[,c(2:31)]
testdt4 <- testexp4[,c(2:30)]
# grow tree 
fitdt <- rpart(Footfall ~ ., data = traindt4 ,method = "anova",control = rpart.control(minbucket = 60))
prp(fitdt)
summary(fitdt)
###pruning and cv
cpdt = fitdt$cptable[which.min(fitdt$cptable[,"xerror"]),"CP"]
ptree<- prune(fitdt, cp= cpdt)
#Predict Output 
predicteddt4 = predict(ptree,testdt4)
testexp4$Footfall <- predicteddt4
testdtunique2 <- rbind(testexp1,testexp2,testexp3,testexp4)
testdtunique <- rbind(testdtunique1,testdtunique2)

submission5 <- testdtunique[c("ID","Footfall")]
write.csv(submission5 , "submission5.csv")


####tukka 
submission5 <- merge(submissiondt,submission5,by = "ID")
submission5 <- merge(submission5,submissionlasso, by = "ID")
submission5$Footfall <- (((submission5$Footfall.x)*(submission5$Footfall.y)*(submission5$Footfall))^.33)*1.055
submissiont <- submission5[c(1,4)]

write.csv(submissiont , "submissiont.csv")



#random forest
library(randomForest)
randomfit1 <- randomForest(Footfall~ month + week + days days + Min_Moisture_In_Park + Range_Moisture + Direction_Of_Wind + Min_Moisture_In_Parkquant,data=traindt1,nodesize=25,ntree=100)
randomfit2 <- randomForest(Footfall~.,data=traindt2,nodesize=25,ntree=100)
randomfit3 <- randomForest(Footfall~.,data=traindt3,nodesize=25,ntree=100)
randomfit4 <- randomForest(Footfall~.,data=traindt4,nodesize=25,ntree=100)



summary(randomfit1)
predicted1 = predict(randomfit1,testdt1)
predicted2 = predict(randomfit2,testdt2)
predicted3 = predict(randomfit3,testdt3)
predicted4 = predict(randomfit4,testdt4)

pred1 = as.data.frame(predicted1)
pred2 = as.data.frame(predicted2)
pred3 = as.data.frame(predicted3)
pred4 = as.data.frame(predicted4)

testexp1$Footfall = pred1$predicted1
testexp2$Footfall = pred2$predicted2
testexp3$Footfall = pred3$predicted3
testexp4$Footfall = pred4$predicted4

testrfunique =rbind(testexp1,testexp2,testexp3,testexp4)

submission5 <- testrfunique[c("ID","Footfall")]
write.csv(submission5 , "submission5.csv")




###tuning
library(caret)
library(randomForest)
library(gbm)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(1)
tunegrid <- expand.grid(.mtry=c(1:21))
rf_gridsearch <- train(Footfall~., data=traindt1, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
View(exp)
fit <- randomForest(exp$Footfall~.,data=exp,nodesize=25,ntree=200)

summary(randomfit1)
predicted1 = predict(randomfit1,testdt1)
predicted2 = predict(randomfit2,testdt2)
predicted3 = predict(randomfit3,testdt3)
predicted4 = predict(randomfit4,testdt4)

pred1 = as.data.frame(predicted1)
pred2 = as.data.frame(predicted2)
pred3 = as.data.frame(predicted3)
pred4 = as.data.frame(predicted4)

testexp1$Footfall = pred1$predicted1
testexp2$Footfall = pred2$predicted2
testexp3$Footfall = pred3$predicted3
testexp4$Footfall = pred4$predicted4

testrfunique =rbind(testexp1,testexp2,testexp3,testexp4)

submission5 <- testrfunique[c("ID","Footfall")]
write.csv(submission5 , "submission5.csv")


# Example of Stacking algorithms
install.packages("caretEnsemble")
library(caret)
library(caretEnsemble)
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = "final")
algorithmList <- c('lm', 'rpart', 'lars', 'glmnet' , "knn")
set.seed(200)
models <- caretList(Footfall~., data=traindt1, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)
modelCor(results)
splom(results)
# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(200)
stack.glm <- caretStack(models, method="lm", metric = "rmse", trControl=stackControl)
print(stack.glm)



# stack using random forest
set.seed(200)
stack.rf <- caretStack(models, method="rf", metric="RMSE", trControl=stackControl)
print(stack.rf)



#2


models <- caretList(Footfall~., data=traindt2, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)
modelCor(results)
splom(results)
# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(200)
stack.glm <- caretStack(models, method="lm", metric = "rmse", trControl=stackControl)
print(stack.glm)






install.packages("party")
install.packages("randomForest")
install.packages("ROCR")
library(randomForest)
library(ROCR)
library(randomForest)
set.seed(71) 
xrf <- train_new[,c(2,4:17,19:24)]
rf <-randomForest(Footfall~xrf,train_new, ntree=20, mtry = 4) 
print(rf)










xrf <- train_new[-c(1,18,17)]
testnewrf <- test_new[-c(1,18,17)]



# Fitting model
fitrf <- randomForest(Footfall ~ ., trainnewrf,ntree=1 )
summary(fitrf)
#Predict Output 

predicted= predict(fitrf,testnewrf)











# load the package
install.packages("glmnet")
library(glmnet)
# fit model
fitelasticnet <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
summary(fitelasticnet)
# make predictions
predictionselastic <- predict(fitelasticnet, x, type="link")
# summarize accuracy
rmseelasticnet <- (mean((y - predictionselastic)^2))^0.5

print(rmseelasticnet)

predictionselastictestx <- predict(fitelasticnet, testx, type="link")
test_new$Footfall <- predictionselastictestx
submission2 <- test_new[c("ID","Footfall")]
write.csv(submission2, "submission2.csv")






#linear
x <- as.data.frame(train_new[,c(4:17,20:23)])
y <- as.data.frame(train_new[3])
linear <- lm(y~x, train_new)
summary(linear)
predictionslm <- predict(linear,testx)





### Load the library
library(e1071)
install.packages("caret")
library(caret)


# Load the iris dataset


# Define training control: 5 fold cross-validation. If you want to perform 10 fold cv, set number=10,
train_control <- trainControl(method="cv", number=5)

# Train the model using randomForest (rf)
modelrfcv <- train(Footfall~., data=trainnew, train_control = train_control, method="rf")

##The printed summary shows the sample sizes used, the best model selected and other information.
print(modelrfcv)

# Make predictions
predictions <- predict(model, iris[,-1])

# Summarize results
result <- data.frame(Actual=iris[,1],Predicted=predictions)
result$Difference <- abs(result$Actual – result$Predicted)
summary(result$Difference)

## The End