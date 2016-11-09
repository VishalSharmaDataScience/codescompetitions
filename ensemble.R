#1
FinalData <- testtrain1[,-4]
FinalData <- sapply(FinalData, as.numeric)
FinalData <- as.data.frame(FinalData)
newTrain <- FinalData[1:nrow(train_new1),]
newTest <- FinalData[(nrow(train_new1)+1):nrow(testtrain1),]
newTrain <- newTrain[-influential,]

y <- as.numeric(newTrain[, "Footfall"])
newTrain <- newTrain[,- 30]
newTest <- newTest[,- 30]

# Find factor variables and translate to numeric
f <- c()
for(i in 1:ncol(newTrain)) {
  if (is.factor(newTrain[, i])) f <- c(f, i)
}

f.t <- c()
for(i in 1:ncol(newTest)) {
  if (is.factor(newTest[, i])) f.t <- c(f.t, i)
}

ttrain <- rbind(newTrain, newTest)
for (i in f) {
  ttrain[, i] <- as.numeric(ttrain[, i]) 
}

x.train <- ttrain[1:nrow(newTrain), ]
x.test <- ttrain[(nrow(newTrain)+1):nrow(ttrain), ]


library(xgboost)

doTest <- function(y, train, test, param0, iter) {
  n<- nrow(train)
  xgtrain <- xgb.DMatrix(as.matrix(train), label = y ,missing = NaN)
  xgval = xgb.DMatrix(as.matrix(test) , missing = NaN)
  watchlist <- list('train' = xgtrain)
  model = xgb.train(
    nrounds = iter
    , params = param0
    , data = xgtrain
    , watchlist = watchlist
    , print.every.n = 100
    , nthread = 12
  )
  p <- predict(model, xgval)
  rm(model)
  gc()
  p
}

param0 <- list(
  # general , non specific params - just guessing
  "objective"  = "reg:linear"
  , "eval_metric" = "rmse"
  , "eta" = 0.25
  , "subsample" = 0.68
  , "colsample_bytree" = 0.78
  , "min_child_weight" = 1
  , "max_depth" = 12
  , "missing" = NaN
)

ensemble <- rep(0, nrow(test_new1))
# change to 1:5 to get result
for (i in 1:5) {
  p <- doTest(y, newTrain[,-c(1,5,6,8:11,13)], newTest[-c(1,5,6,8:11,13)], param0, 600) 
  # change to 1300 or 1200, test by trial and error, have to add to local check which suggests 900, 
  # but have another 20% training data to concider which gives longer optimal training time
  ensemble <- ensemble + p
}


test_new1$Footfall <- (ensemble)/5



FinalData <- testtrain2[,-4]

newTrain <- FinalData[1:nrow(train_new2),]
newTest <- FinalData[(nrow(train_new2)+1):nrow(testtrain2),]


y <- as.numeric(newTrain[, "Footfall"])
newTrain <- newTrain[,- 30]
newTest <- newTest[,- 30]

# Find factor variables and translate to numeric
f <- c()
for(i in 1:ncol(newTrain)) {
  if (is.factor(newTrain[, i])) f <- c(f, i)
}

f.t <- c()
for(i in 1:ncol(newTest)) {
  if (is.factor(newTest[, i])) f.t <- c(f.t, i)
}

ttrain <- rbind(newTrain, newTest)
for (i in f) {
  ttrain[, i] <- as.numeric(ttrain[, i]) 
}

x.train <- ttrain[1:nrow(newTrain), ]
x.test <- ttrain[(nrow(newTrain)+1):nrow(ttrain), ]


library(xgboost)

doTest <- function(y, train, test, param0, iter) {
  n<- nrow(train)
  xgtrain <- xgb.DMatrix(as.matrix(train), label = y ,missing = NaN)
  xgval = xgb.DMatrix(as.matrix(test) , missing = NaN)
  watchlist <- list('train' = xgtrain)
  model = xgb.train(
    nrounds = iter
    , params = param0
    , data = xgtrain
    , watchlist = watchlist
    , print.every.n = 100
    , nthread = 12
  )
  p <- predict(model, xgval)
  rm(model)
  gc()
  p
}

param0 <- list(
  # general , non specific params - just guessing
  "objective"  = "reg:linear"
  , "eval_metric" = "rmse"
  , "eta" = 0.25
  , "subsample" = 0.68
  , "colsample_bytree" = 0.78
  , "min_child_weight" = 1
  , "max_depth" = 12
  , "missing" = NaN
)

ensemble <- rep(0, nrow(test_new2))
# change to 1:5 to get result
for (i in 1:5) {
  p <- doTest(y, newTrain[,-c(1,5,6,8:11,13)], newTest[-c(1,5,6,8:11,13)], param0, 600) 
  # change to 1300 or 1200, test by trial and error, have to add to local check which suggests 900, 
  # but have another 20% training data to concider which gives longer optimal training time
  ensemble <- ensemble + p
}


test_new2$Footfall <- (ensemble)/5












test_new <- rbind(test_new1,test_new2)

submissionxggm1 <- test_new[c("ID","Footfall")]
write.csv(submissionxggm1,"submissionxggm1.csv")
###tukka 

submission5 <- merge(sub2,sub1, by = "ID")

submission5 <- merge(submission5,submissionxggm1,by = "ID")
submission5$Footfall <- (((submission5$Footfall.x)*(submission5$Footfall.y)*(submission5$Footfall))^.33)*1.051
submissiont <- submission5[c(1,5)]
write.csv(submissiont,"submissiont.csv")


submission5 <- merge(sub1,submissionstack, by = "ID")
submission5$Footfall <- (submission5$Footfall.x *submission5$Footfall.y)^.5
submissiont <- submission5[c(1,4)]
write.csv(submissiont,"submissiont.csv")
