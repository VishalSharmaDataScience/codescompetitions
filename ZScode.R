library(h2o)
library(data.table)
setwd()
train = read.csv("train.csv", stringsAsFactors = T)
test = read.csv("test.csv",stringsAsFactors = T)
samplesub = read.csv("samplesub.csv",stringsAsFactors = T)
str(train)
utils::View(train)
sub_mean <- data.frame(agentID = test$agentID,  attrite = logical(mode(train$attrite)))
write.csv(sub_mean, file = "first_sub.csv", row.names = F)
table(train$attrite)
summary(test)
test$attrite = 0
cc = list(test,train)
combi = rbindlist(cc)

 


summary(test)
train$attrite = as.factor(train$attrite)
test$attrite = as.factor(test$attrite)


###
colSums(is.na(train) > 0)
train$feature77 = NULL
test$feature77 = NULL
train$feature127 = NULL
test$feature127= NULL
table(train$feature69)
train$feature69 = as.numeric(train$feature69)
test$feature69  = as.numeric(test$feature69)
cor = as.data.frame(cor(train[2:169]))
train$feature103 =  NULL
test$feature103 = NULL
table(train$feature69,train$attrite)
combi = combi[sapply(combi, function(x) !is.factor(x) | length(unique(x))> 1 )]
str(train)
train[c(2:69,71:168)] = scale(train[c(2:69,71:168)],center = T ,scale = T)
test[c(2:69,71:168)] = scale(test[c(2:69,71:168)],center = T ,scale = T)

####
train[] = train[sapply(train,function(x) length(unique(x))> 1)]



train = train[sapply(train,function(x) length(unique(x))> 1)]
test = test[sapply(test,function(x) length(unique(x))> 1)]
test$attrite = 0
test$attrite = as.factor(test$attrite)
#####RF


# Load library
library(randomForest)
# Help on ramdonForest package and function
library(help=randomForest)
help(randomForest)


sample.ind <- sample(2, 
                     nrow(train),
                     replace = T,
                     prob = c(0.6,0.4))
cross.sell.dev <- train[sample.ind==1,]
cross.sell.val <- train[sample.ind==2,]

table(cross.sell.dev$attrite)/nrow(cross.sell.dev)


varNames <- names(cross.sell.dev)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("attrite","agentID")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("attrite", varNames1, sep = " ~ "))



cross.sell.rf <- randomForest(rf.form,
                              cross.sell.dev,
                              ntree=500,
                              importance=T)

plot(cross.sell.rf)
#ble Importance in R

# Variable Importance Plot
varImpPlot(cross.sell.rf,
           sort = T,
           main="Variable Importance",
           n.var=5)

# Variable Importance Plot
varImpPlot(cross.sell.rf,
           sort = T,
           main="Variable Importance",
           n.var=8)


# Variable Importance Table
var.imp <- data.frame(importance(cross.sell.rf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

# Predicting response variable
h <- as.data.frame(predict(cross.sell.rf ,cross.sell.dev,type = "prob"))
summary(cross.sell.dev$predicted.response)


summary(cross.sell.dev)
# Load Library or packages
library(e1071)
library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
# Create Confusion Matrix
confusionMatrix(data=cross.sell.dev$predicted.response,
                reference=cross.sell.dev$attrite)
#Validation Sample

# Predicting response variable
cross.sell.val$predicted.response <- predict(cross.sell.rf ,cross.sell.val)

# Create Confusion Matrix
confusionMatrix(data=cross.sell.val$predicted.response,
                reference=cross.sell.val$attrite)
             
test$attrite = predict(cross.sell.rf,test[2:168])
table(test$attrite)
write.csv(test,"submission.csv",row.names = F)


#####SVM

## Split into development and validation sample

sample.ind <- sample(2, 
                     nrow(train),
                     replace = T,
                     prob = c(0.6,0.4))
svm.develop <- train[sample.ind==1,]
svm.validate <- train[sample.ind==2,]

table(svm.validate$attrite)/length(svm.validate$attrite)


l and Load SVM Package in R

# Install SVM package in R
#install.packages("e1071")

# Load SVM package
require(e1071)
## Loading required package: e1071
# Help 
#library(help=e1071)

# Install SVM package in R
#install.packages("e1071")

# Load SVM package
require(e1071)
## Loading required package: e1071
# Help 
#library(help=e1071)

# SVM model using R
svm.model <- svm(attrite~. - agentID,
                 data=svm.develop)

# Summary of the model
summary(svm.model)

#ict using Support Vector Machine

# Predict Target Label
svm.validate$Predicted_y <- predict(svm.model, 
                                    svm.validate)
table(svm.validate$Predicted_y)

# Compare Observed and Predicted
table.svm <- table(pred = svm.validate$Predicted_y,
                   true = svm.validate$attrite)/length(svm.validate$attrite)
table.svm
test$attrite = predict(svm.model,test[1:168],)
table(test$attrite)
write.csv(test,"submissionsvm.csv",row.names = F)
####PCA

pca.train <- train
pca.test = test

pca.train <- pca.train[-c(169)]
pca.test <- pca.test[-c(169)]

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
train.data <- data.frame(attrite = train$attrite, prin_comp$x)
train.data <- train.data[,1:21]

library(randomForest)

rf.model <- randomForest(attrite ~.  , data = train.data,method = "class"  , ntree = 100 , nodesize= 25)


##transform test
test.data <- predict(prin_comp ,newdata = pca.test)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:21]

#make prediction
rpart.prediction <- predict(rf.model,test.data)
predictionpca <- as.data.frame(rpart.prediction)

pca.test$attrite <- predictionpca$rpart.prediction
pca.test <- pca.test[c("agentID","attrite")]
table(pca.test$attrite)

#####MLR
#create a task
trainTask <- makeClassifTask(data = train,target = "attrite")
testTask <- makeClassifTask(data = test, target = "attrite")
trainTask
trainTask <- makeClassifTask(data = train,target = "attrite", positive = 1)
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")
trainTask <- dropFeatures(task = trainTask,features = c("agentID","attrite"))
logistic.learner <- makeLearner("classif.logreg",predict.type = "response")
cv.logistic <- crossval(learner = logistic.learner,task = trainTask,iters = 3,stratify = TRUE,measures = acc,show.info = F)



####outliers 


outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}


#imbalanced classification
prop.table(table(train$attrite))
library(rpart)
treeimb <- rpart(attrite ~ . -agentID, data = train)
pred.treeimb <- predict(treeimb, newdata = test[1:186])
sub_rpartbasic = sub_mean
sub_rpartbasic$attrite = pred.treeimb
summary(sub_rpartbasic)
install.packages("ROSE")
library(ROSE)
install.packages("unbalanced")
install.packages("DMwR")
library(DMwR)

trainsmote <- SMOTE(attrite ~ ., train, perc.over = 100, perc.under=200)
prop.table(table(trainsmote$attrite))
