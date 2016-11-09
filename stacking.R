testtrainold <- read.csv("testtrainoldbeforetransformation.csv")
testtrainold <- testtrainold[-1]

testtrainold$Direction_Of_Wind[is.na(testtrainold$Direction_Of_Wind)] = mean(testtrainold$Direction_Of_Wind, na.rm = T)
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

####

install.packages("caretEnsemble")
library(caret)
library(caretEnsemble)
# create submodels
control <- trainControl(method="repeatedcv", number=5, repeats=2, savePredictions = "final")
algorithmList <- c('bstSm', 'earth')
set.seed(200)
models <- caretList(Footfall ~ ., data=train_new2[c("month" , "week" , "Min_Moisture_In_Park" ,  "Average_Breeze_Speed" ,"Footfall"   )], trControl=control, methodList=algorithmList)
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
stack.cart <- caretStack(models, method="rpart", metric="RMSE", trControl=stackControl)
print(stack.cart)


predictstack <- predict(stack.glm,test_new2)



test_new2$Footfall <- predictstack
test_new <- rbind(test_new1,test_new2)
submissionstack <- test_new[c("ID","Footfall")]
write.csv(submissionstack ,"submissionstack.csv")




###splines ##
library(party)
set.seed(415)
fit <- cforest(Footfall ~ month + Min_Moisture_In_Park + Max_Moisture_In_Park + Average_Moisture_In_Park + days +  Var1 + week + Average_Breeze_Speed + Min_Breeze_Speed + Max_Breeze_Speed + Min_Ambient_Pollution + Max_Ambient_Pollution,
                 data = train_new1, 
                 controls=cforest_unbiased(ntree=200, mtry=3))


