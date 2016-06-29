library(data.table)
library(ggplot2)
library(randomForest)
library(e1071)
library(caret)
library(foreach)
library(doMC)

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
top10 <- train[train$Category %in% counts$Category[1:10],]
top10$Category <- as.factor(as.character(top10$Category))

n <- names(df)
f <- as.formula(paste("Category ~","X+Y+Year+Month+Hour+",paste(n[grep("^DayOfWeek",n)],collapse = "+")))

index.train <- sample(1:nrow(top10),round(0.1*nrow(top10)),replace = FALSE)
train.top10 <- top10[index.train,]

#nrow(train.top10)/nrow(df)

gc()

fitGrid  <-  expand.grid(mtry= 1:4)
fitControl <- trainControl(method = "cv",  number = 10)

system.time(model.tune <-train(f,data=train.top10,trace=F,method="rf", 
                               trControl = fitControl,tuneGrid = fitGrid))

#model.tune
#confusionMatrix(model.tune)

trellis.par.set(caretTheme())
ggplot(model.tune,plotType = "scatter")

system.time(forest.model <- randomForest(f, data=train.top10, mtry=model.tune$bestTune$mtry))
#gc()

#forest.model

layout(matrix(c(1,2),nrow=1),width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(forest.model,log="y")
par(mar=c(5,0,4,0)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top",colnames(forest.model$err.rate),col=1:10,cex=0.6,fill=1:10)
dev.off() 

varImpPlot(forest.model)

gc()

split_testing<-sort(rank(1:nrow(test))%%4)

pred.forest <- foreach(i=unique(split_testing),  
               .combine=c,.packages=c("randomForest")) %dopar% {  
               predict(forest.model,newdata=test[split_testing==i,],type="class")  
               } 

train.levels <- levels(train.top10$Category)
attr(pred.forest,"names") <- NULL

for (i in 1:length(train.levels)){
  pred.forest[which(pred.forest == i)] <- train.levels[i]  
}

sum(ifelse(pred.forest == ans, 1, 0))/length(ans)















  
  
  



