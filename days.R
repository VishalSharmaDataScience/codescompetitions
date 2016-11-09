setwd("C:\\Users\\inspiron\\Desktop\\RPractice")

master <- read.csv("mastert.csv")
masterd <- master[c(2,11,3,14,12,13)]
rm(master)
View(masterd)
table(masterd$Gender)

masterd <- subset(masterd,masterd$Gender != "")
date <- transform(masterd, test=do.call(rbind, strsplit(as.character(masterd$Member.Since), ' ', fixed=TRUE)), stringsAsFactors=F)
colnames(date)[7] <- "MemberSinceDate"
colnames(date)[8] <- "MemberSinceTime"
date$MemberSinceDate <-  as.Date(date$MemberSinceDate , "%d-%m-%Y")
date$memberdays <- as.Date("2016-05-01")
date$ndays <- date$memberdays -date$MemberSinceDate

date <- date[-c(8,7,6)]
table(is.na(date$ndays))
date <- subset(date , is.na(date$ndays) != T)

cb <- read.csv("sampled-cashback.csv")
cb <- cb [c(4,14)]
cb$Date.Asked <- as.Date(cb$Date.Asked, "%d-%m-%Y" )
library(plyr)
date <- join(masterd,cb, type = "inner",  by = "User.ID" )
date2 <- unique(date)
rm(masterd,cb,date)
View(date2)

library(tidyr)





date$Date.Asked = strptime(date$Date.Asked, format="%d-%m-%y")
date$MemberSinceDate = strptime(date$MemberSinceDate, format="%d-%m-%y")
View(date)
date$MemberSinceDate = strptime(date$MemberSinceTime, format="%H:%M")
date$Weekday = weekdays(date$Date.Asked)
date$Date.Asked = strptime(date$Date.Asked, format="%d-%m-%Y")
date$Date.Asked = strptime(date$Date.Asked, format="%d-%m-%y")
date <- date[-6]
table(date$Weekday)
WeekdayCounts = as.data.frame(table(date$Weekday))
library(ggplot2)
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday"))
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total Transactions")
View(date)
View(date)
table(mvt$Weekday, date$Details)
table(date$Weekday, date$Details)
DayHourCounts = as.data.frame(table(date$Weekday, date$Details))
retailers <- subset(DayHourCounts, DayHourCounts$Var2 == "Amazon" | DayHourCounts$Var2 == "Snapdeal" | DayHourCounts$Var2 == "ShopClues" | DayHourCounts$Var2 == "Flipkart" | DayHourCounts$Var2 == "Abof" | DayHourCounts$Var2 == "AliExpress" )
