######extracting exp##########
exp = train_new[,4:27]
exp$weekday = wday(exp$date)
exp = exp[,-c(14)]
######XGboost#####
install.packages("xgboost")
library(xgboost)
library(readr)
library(stringr)
library(caret)

y <- xgb.DMatrix()
exp <- as.matrix(exp)
exp <- xgb.DMatrix(exp)
xgb <- xgboost(data = exp, 
               label = ,
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               set.seed = 1,
               eval_metric = "merror",
               objective = "reg:linear",
               num_class = 12,
               nthread = 3
)

y_pred <- predict(xgb, data.matrix(X_test[,-1]))


########gbm##############
install.packages("caret")
library(caret)
library(gbm)

fitControl <- trainControl(method = "cv",
                           number = 10)

gbmGrid=expand.grid(interaction.depth = 2,
                    n.trees = 500,
                    shrinkage = 0.1,
                    n.minobsinnode = 10)
set.seed(825)
fit <- train(Footfall~.,data=exp,
             method = "gbm",
             trControl = fitControl,
             verbose = FALSE,
             tuneGrid = gbmGrid)
fit
summary(fit)

fit <- train(Footfall~rangemoisture+Min_Moisture_In_Park+Direction_Of_Wind+Max_Atmospheric_Pressure,data=exp,
             method = "gbm",
             trControl = fitControl,
             verbose = FALSE,
             tuneGrid = gbmGrid)








library(lubridate)
exp = train_new[,4:27]
exp$weekday = wday(exp$date)
exp = exp[,-c(14)]
View(exp)

yes

predicted= predict(fit,test,type= "prob")[,2]

exp = as.data.frame(exp)