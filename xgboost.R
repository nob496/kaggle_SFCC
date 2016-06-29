#library(devtools)
library(xgboost)
library(data.table)
library(ggplot2)
library(reshape2)
library(Ckmeans.1d.dp)
#library(xgb.plot.tree)
#library(caret)
#library(foreach)
#library(doMC)

#cl <- 4
#registerDoMC(cl)

df <- fread("train.modified.csv", stringsAsFactors=TRUE)
df <- df[,V1:=NULL]

index <- sample(1:nrow(df),round(0.5*nrow(df)),replace = FALSE)
train <- df[index,]

#counts <- df[,list(Counts = length(X)),by=Category]
#counts <- counts[order(-counts$Counts),]

#train <- train[train$Category %in% counts$Category[1:10],]
#train$Category <- as.factor(as.character(train$Category))

test <- df[-index,]

train <- as.data.frame(train)
train.x = train[,1:12]
train.y = train$Category
train.y <- as.integer(train.y)-1

test <- as.data.frame(test)
test.x = test[,1:12]
test.y = test$Category
test.y <- as.integer(test.y)-1

x <- rbind(train.x,test.x)
x <- as.matrix(x)

trind <- 1:length(train.y) #designating train data
teind <- (nrow(train.x)+1):nrow(x) #designating test data

set.seed(131) 
param <- list("objective" = "multi:softprob", # get probablities for multi classification
              "eval_metric" = "mlogloss", # set loss function
              "num_class" = 39 # the number of class
)
#param <- list("objective" = "multi:softprob", # get probablities for multi classification
#              "eval_metric" = "mlogloss", # set loss function
#              "num_class" = 10 # the number of class
#)

k <- round(1+log2(nrow(train.x)))

cv.nround <- 100 #search
bst.cv <- xgb.cv(param=param, data = x[trind,], label = train.y, nfold = k, nrounds=cv.nround)
bst.cv$ntree <- 1:nrow(bst.cv)

bst.cv2 <- melt(bst.cv, id.vars = "ntree",measure.vars = c("train.mlogloss.mean","test.mlogloss.mean"))
  
g <- ggplot(bst.cv2, aes(x = ntree, y = value, group = variable, colour = variable))
g <- g + geom_line()
g <- g + geom_point()
g <- g + scale_y_continuous(breaks=c(2.2,2.4,2.6,2.8,3.0,3.2),limits=c(2.2,3.2))
g <- g + xlab("nrounds") + ylab("logloss")
plot(g)

param <- list("objective" = "multi:softmax","eval_metric" = "mlogloss","num_class" = 39)
#param <- list("objective" = "multi:softmax","eval_metric" = "mlogloss","num_class" = 10)

set.seed(131)
nround <- 100

bst <- xgboost(param=param, data = x[trind,], label = train.y, nrounds=nround)

pred <- predict(bst,x[teind,])

#train.levels <- levels(train$Category)
#for (i in 1:length(train.levels)){
#  pred[which(pred == i-1)] <- train.levels[i]  
#}

sum(ifelse(pred == test.y, 1, 0))/length(test.y)
sum(ifelse(pred[which(test.y == 16)] == test.y[test.y == 16], 1, 0))/length(test.y[test.y == 16]) #LARCENY/THEFT

imp<-xgb.importance(names(df),model=bst)
print(imp)
xgb.plot.importance(imp)



