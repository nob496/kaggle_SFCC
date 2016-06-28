library(data.table)
library(dplyr)
library(lubridate)
library(caret)

data.train <- read.csv("train.csv")
data.test <- read.csv("test.csv")

data.train <- transform(data.train, DayOfWeek= factor(DayOfWeek,
                                                      levels = c("Monday","Tuesday","Wednesday","Thursday","Friday",
                                                                 "Saturday","Sunday")))

data.test <- transform(data.test, DayOfWeek= factor(DayOfWeek,
                                                    levels = c("Monday","Tuesday","Wednesday","Thursday","Friday",
                                                               "Saturday","Sunday")))

#OlsonNames()
data.train$Dates <- as.POSIXlt(data.train$Dates,format="%Y-%m-%d %H:%M:%S",tz = "America/Los_Angeles",usetz=TRUE)
data.train$Year <- year(data.train$Dates)
data.train$Month <- month(data.train$Dates)
data.train$Day <- yday(data.train$Dates)
data.train$Hour <- hour(data.train$Dates)

data.test$Dates <- as.POSIXlt(data.test$Dates,format="%Y-%m-%d %H:%M:%S",tz = "America/Los_Angeles",usetz=TRUE)
data.test$Year <- year(data.test$Dates)
data.test$Month <- month(data.test$Dates)
data.test$Day <- yday(data.test$Dates)
data.test$Hour <- hour(data.test$Dates)

data.train <- data.train[, colnames(data.train)!="Dates"]
data.test <- data.test[, colnames(data.test)!="Dates"]

data.train <- as.data.table(data.train,keep.rownames = TRUE)
data.train$rn = as.numeric(data.train$rn) 
setkey(data.train,rn)

data.test <- as.data.table(data.test,keep.rownames = TRUE)
data.test$rn = as.numeric(data.test$rn) 
setkey(data.test,rn)

data.train <- data.train[X != -120.5 & Y != 90]
data.test <- data.test[X != -120.5 & Y != 90]

#DataTrainAddr.med = data.train[,list(X=median(X),Y=median(Y)),by=Address]
#DataTrainPdDis.med = data.train[,list(X=median(X),Y=median(Y)),by=PdDistrict]

#i <- NULL
#j <- NULL
#k <- NULL
#raddr <- NULL
#loop <- NULL

#loop <- nrow(data.train[X == -120.5 & Y == 90])
#raddr <- data.train[,rn[X == -120.5 & Y == 90]]

#for(i in 1:loop){ 
#  j <- which(DataTrainAddr.med[,Address] == data.train[,Address][raddr[i]])
#  k <- which(DataTrainPdDis.med[,PdDistrict] == data.train[,PdDistrict][raddr[i]])
  
#  if (DataTrainAddr.med$X[j] != -120.5 & DataTrainAddr.med$Y[j] != 90)
#  {
#    data.train$X[raddr[i]] = DataTrainAddr.med$X[j]
#    data.train$Y[raddr[i]] = DataTrainAddr.med$Y[j]
#  }
#  else {
#    data.train$X[raddr[i]] = DataTrainPdDis.med$X[k]
#    data.train$Y[raddr[i]] = DataTrainPdDis.med$Y[k]
#  }
#}

#DataTestAddr.med = data.test[,list(X=median(X),Y=median(Y)),by=Address]
#DataTestPdDis.med = data.test[,list(X=median(X),Y=median(Y)),by=PdDistrict]

#i <- NULL
#j <- NULL
#k <- NULL
#raddr <- NULL
#loop <- NULL

#loop <- nrow(data.test[X == -120.5 & Y == 90])
#raddr <- data.test[,rn[X == -120.5 & Y == 90]]

#for(i in 1:loop){ 
#  j <- which(DataTestAddr.med[,Address] == data.test[,Address][raddr[i]])
#  k <- which(DataTestPdDis.med[,PdDistrict] == data.test[,PdDistrict][raddr[i]])
  
#  if (DataTestAddr.med$X[j] != -120.5 & DataTestAddr.med$Y[j] != 90)
#  {
#    data.test$X[raddr[i]] = DataTestAddr.med$X[j]
#    data.test$Y[raddr[i]] = DataTestAddr.med$Y[j]
#  }
#  else {
#    data.test$X[raddr[i]] = DataTestPdDis.med$X[k]
#    data.test$Y[raddr[i]] = DataTestPdDis.med$Y[k]
#  }
#}

# scale numerical variables for train data (0 ~ 1)
data.train$X <- scale(data.train$X, center = min(data.train[,X]), scale = max(data.train[,X]) - min(data.train[,X]))
data.train$Y <- scale(data.train$Y, center = min(data.train[,Y]), scale = max(data.train[,Y]) - min(data.train[,Y]))
data.train$Year <- scale(data.train$Year, center = min(data.train[,Year]), scale = max(data.train[,Year]) - min(data.train[,Year]))
data.train$Month <- scale(data.train$Month, center = min(data.train[,Month]), scale = max(data.train[,Month]) - min(data.train[,Month]))
data.train$Day <- scale(data.train$Day, center = min(data.train[,Day]), scale = max(data.train[,Day]) - min(data.train[,Day]))
data.train$Hour <- scale(data.train$Hour, center = min(data.train[,Hour]), scale = max(data.train[,Hour]) - min(data.train[,Hour]))

data.train$X <-  as.numeric(data.train$X)
data.train$Y <- as.numeric(data.train$Y)
data.train$Year <- as.numeric(data.train$Year)
data.train$Month <- as.numeric(data.train$Month)
data.train$Day <- as.numeric(data.train$Day)
data.train$Hour <- as.numeric(data.train$Hour)

# scale numerical variables for test data (0 ~ 1)
data.test$X <- scale(data.test$X, center = min(data.test[,X]), scale = max(data.test[,X]) - min(data.test[,X]))
data.test$Y <- scale(data.test$Y, center = min(data.test[,Y]), scale = max(data.test[,Y]) - min(data.test[,Y]))
data.test$Year <- scale(data.test$Year, center = min(data.test[,Year]), scale = max(data.test[,Year]) - min(data.test[,Year]))
data.test$Month <- scale(data.test$Month, center = min(data.test[,Month]), scale = max(data.test[,Month]) - min(data.test[,Month]))
data.test$Day <- scale(data.test$Day, center = min(data.test[,Day]), scale = max(data.test[,Day]) - min(data.test[,Day]))
data.test$Hour <- scale(data.test$Hour, center = min(data.test[,Hour]), scale = max(data.test[,Hour]) - min(data.test[,Hour]))

data.test$X <-  as.numeric(data.test$X)
data.test$Y <- as.numeric(data.test$Y)
data.test$Year <- as.numeric(data.test$Year)
data.test$Month <- as.numeric(data.test$Month)
data.test$Day <- as.numeric(data.test$Day)
data.test$Hour <- as.numeric(data.test$Hour)

#create dummy variables from factors for train data (Category and DayofWeek)
#noNames <- dummyVars(~Category+X+Y+Year+Month+Hour+DayOfWeek,data=data.train)
#data.train.dummy <- as.data.frame(predict(noNames, data.train))

#data.sample <- read.csv("sampleSubmission.csv")

#names.sample <- colnames(data.sample)
#names.sample <- names.sample[-1] 
#names.train <- colnames(data.train.dummy)[grep("^Category",colnames(data.train.dummy))]
#names <- data.frame("sample" = names.sample,"train" = names.train)
#colnames(data.train.dummy)[grep("^Category",colnames(data.train.dummy))] <- names.sample
#write.csv(data.train.dummy, file = "train.modi.csv")

noNames <- dummyVars(~X+Y+Year+Month+Hour+DayOfWeek,data=data.train)
data.train.dummy <- as.data.frame(predict(noNames, data.train))
data.train.dummy$Category <- data.train$Category

write.csv(data.train.dummy, file = "train.modified.csv")

# create dummy variables from factors for test data (Category and DayofWeek)
noNames <- dummyVars(~X+Y+Year+Month+Hour+DayOfWeek,data=data.test)
data.test.dummy <- as.data.frame(predict(noNames, data.test))

write.csv(data.test.dummy, file = "test.modified.csv")
