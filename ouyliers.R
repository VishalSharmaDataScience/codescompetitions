##outliers


mod <- lm(Footfall ~  , data=train_new)
summary(mod)

predictionspline <- predict()
month + Min_Moisture_In_Park + Max_Moisture_In_Park + Average_Moisture_In_Park + days +  Var1 + week + Average_Breeze_Speed + Min_Breeze_Speed + Max_Breeze_Speed + Min_Ambient_Pollution + Max_Ambient_Pollution

cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")

text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red") 
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
head(train_new[influential, ])  # influential observations.

#######
testtrain <- testtrain[,5:16]
kmeans.result <- kmeans(testtrain, centers=2)
# cluster centers
kmeans.result$centers
# calculate distances between objects and cluster centers
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((testtrain - centers)^2))
# pick top 5 largest distances
outliers <- order(distances, decreasing=T)
# who are outliers



