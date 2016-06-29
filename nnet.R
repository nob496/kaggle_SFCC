library(dplyr)
library(data.table)
library(ggplot2)
library(nnet)
library(caret)
library(foreach)
library(doMC)

source("nnet_plot_update.R") # made by Peque, https://gist.github.com/Peque/41a9e20d6687f2f3108d

#cl <- detectCores()
#cl<-makeCluster(4)
cl <- 4
registerDoMC(cl)

df <- fread("train.modified.csv", stringsAsFactors=TRUE)
#apply(df,2,function(x) sum(is.na(x)))
df <- df[,V1:=NULL]

index <- sample(1:nrow(df),round(0.5*nrow(df)),replace = FALSE)
train <- df[index,]
test <- df[-index,]

ans <- test[,Category]
test[,Category := NULL]

counts <- df[,list(Counts = length(X)),by=Category]
counts <- counts[order(-counts$Counts),]
train.top10 <- train[train$Category %in% counts$Category[1:10],]
train.top10$Category <- as.factor(as.character(train.top10$Category))

n <- names(df)
f <- as.formula(paste("Category ~","X+Y+Year+Month+Hour+",paste(n[grep("^DayOfWeek",n)],collapse = "+")))

gc()

fitGrid <- expand.grid(decay= c(0, 1e-4, 1e-3,1e-2,1e-1), size = (1:10)*1)
fitControl <- trainControl(method = "cv",  number = 10)

system.time(model.tune　<-　train(f,data=train.top10, method="nnet", 
                                trace=F, trControl = fitControl, tuneGrid = fitGrid))

model.tune$bestTune$size
model.tune$bestTune$decay

trellis.par.set(caretTheme())
plot(model.tune, metric = "Kappa", plotType = "level") 

ggplot(model.tune,plotType = "scatter")

confusionMatrix(model.tune)

system.time(nnet.model <- nnet(f,train.top10,size=model.tune$bestTune$size,decay=model.tune$bestTune$decay))

plot(nnet.model,pos.col='blue',neg.col='red',circle.cex=1)

split_testing<-sort(rank(1:nrow(test))%%4)

pred.nnet <- foreach(i=unique(split_testing),  
                     .combine=c,.packages=c("nnet")) %dopar% {  
                       predict(nnet.model,newdata=test[split_testing==i,],type="class")  
                     } 

sum(ifelse(pred.nnet==ans,1,0))/length(ans)

gc()
