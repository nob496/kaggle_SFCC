library(data.table)
library(ggplot2)
library(LiblineaR)
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

counts <- df[,list(Counts = length(X)),by=Category]
counts <- counts[order(-counts$Counts),]

top10 <- train[train$Category %in% counts$Category[1:10],]
top10$Category <- as.factor(as.character(top10$Category))

top10 <-as.data.frame(top10)

#nrow(top10)/nrow(df)

train.x=top10[,1:12]
train.y=top10$Category

test <-as.data.frame(test)
test.x <- test[,1:12]
test.y <- test$Category

#t<-proc.time()
#m2=LiblineaR(data=train.x,target=train.y,type=2,cross = 2, verbose=FALSE)
#proc.time()-t

# for multi-class classification 
# • 0 – L2-regularized logistic regression (primal)
# • 1 – L2-regularized L2-loss support vector classification (dual)
# • 2 – L2-regularized L2-loss support vector classification (primal)
# • 3 – L2-regularized L1-loss support vector classification (dual)
# • 4 – support vector classification by Crammer and Singer
# • 5 – L1-regularized L2-loss support vector classification
# • 6 – L1-regularized logistic regression
# • 7 – L2-regularized logistic regression (dual)
# for regression • 11 – L2-regularized L2-loss support vector regression (primal)
# • 12 – L2-regularized L2-loss support vector regression (dual)
# • 13 – L2-regularized L1-loss support vector regression (dual)

# Find the best model with the best cost parameter via 10-fold cross-validations
tryTypes=c(0:7)
tryCosts=c(100,10,1,0.1)
type = NA
cost = NA
acc = NA
i = 1

t<-proc.time()
for(ty in tryTypes){
  for(co in tryCosts){
    value = LiblineaR(data=train.x,target=train.y,type=ty,cost=co,cross=2,verbose=FALSE)
    type[i] = ty
    cost[i] = co
    acc[i] = value
    i = i + 1
    value = NULL
    gc()
    }
}
proc.time()-t

result <- data.frame(type,cost,acc)
result$type <- as.factor(result$type)

g <- ggplot(result, aes(x = cost, y = acc, group = type,colour = type))
g <- g + geom_line()
g <- g + geom_point()
g <- g + scale_x_log10(breaks = c(0.1,1,10,100))
plot(g)

result$type[which(result$acc == max(result$acc))]
result$cost[which(result$acc == max(result$acc))]

best.model = LiblineaR(data=train.x,target=train.y,type=6,cost=100,verbose=FALSE)

pred.ln <- predict(best.model,test.x,type="class")

pred <-  as.character(pred.ln$predictions)
ans <- as.character(test.y)

sum(ifelse(pred == ans, 1, 0))/length(ans)





