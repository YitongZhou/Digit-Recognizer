library(caTools)
train = read.csv('train.csv', header=TRUE)
test= read.csv("test.csv",header= T)

split = sample.split(train$label, SplitRatio=.7)
train = subset(train, split==TRUE)
test = subset(train, split==FALSE)

library(rpart)
cart = rpart(label ~ ., data=train, method="class", control = rpart.control(minbucket=10))
cartPredict = predict(cart, newdata=test, type="class") 
head(cartPredict)
cartTable = table(test$label, cartPredict)
sum(diag(cartTable))/nrow(test)

library(randomForest)
rf <- randomforest()
train$label = factor(train$label)
test$label = factor(test$label)

randomForest = randomForest(label ~ .,data=test, nodesize=10, ntree=50)
randomForestPredict = predict(randomForest, newdata=test)
randomForestTable = table(test$label, randomForestPredict)
sum(diag(randomForestTable))/nrow(test)

realTest = read.csv('test.csv')
randomForestPredict = predict(randomForest, newdata=realTest)
out = levels(randomForestPredict)[randomForestPredict] 
write(out, 'prediction.csv')


############
#labels

labels <- as.factor(train[,1])
train.var <- train[,-1]
rf<-randomForest(train.var,labels,xtest=test,ntree = 1000)
plot(rf,main="error rate")
varImpPlot(rf,main = "important variable")

out <- levels(rf$test$predicted)[rf$test$predicted]
write(out,"predictions.csv")

