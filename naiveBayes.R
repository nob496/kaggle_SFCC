library(data.table)
library(ggplot2)
#library(e1071)
library(klaR)
library(caret)
library(foreach)
library(doMC)

cl <- 4
registerDoMC(cl)

df <- fread("train.modified.csv", stringsAsFactors=TRUE)
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

gc()

system.time(bayes.model01.top10 <- NaiveBayes(f, data=top10, usekernel = FALSE))
system.time(bayes.model02.top10 <- NaiveBayes(f, data=top10, usekernel = TRUE))

plot(bayes.model01.top10,legendplot = FALSE)
plot(bayes.model02.top10,legendplot = FALSE)

split_testing<-sort(rank(1:nrow(test))%%4)

pred.bayes01 <- foreach(i=unique(split_testing),  
                .combine=c,.packages=c("klaR")) %dopar% {  
                predict(bayes.model01.top10,newdata=test[split_testing==i,],type="class")  
                }

pred.bayes02 <- foreach(i=unique(split_testing),  
                .combine=c,.packages=c("klaR")) %dopar% {  
                predict(bayes.model02.top10,newdata=test[split_testing==i,],type="class")  
                }

pred.class.bayes01 <- c(pred.bayes01[[1]],pred.bayes01[[3]],pred.bayes01[[5]],pred.bayes01[[7]])

train.levels <- levels(top10$Category)

for (i in 1:length(train.levels)){
  pred.class.bayes01[which(pred.class.bayes01 == i)] <- train.levels[i]  
}

sum(ifelse(pred.class.bayes01 == ans, 1, 0))/length(ans)

pred.class.bayes02 <- c(pred.bayes02[[1]],pred.bayes02[[3]],pred.bayes02[[5]],pred.bayes02[[7]])

train.levels <- levels(top10$Category)

for (i in 1:length(train.levels)){
  pred.class.bayes02[which(pred.class.bayes02 == i)] <- train.levels[i]  
}

sum(ifelse(pred.class.bayes02 == ans, 1, 0))/length(ans)





