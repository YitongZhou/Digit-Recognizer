library(e1071)

train =  read.csv("C:/Users/yiton/Desktop/digit/train.csv", colClasses = c("factor",rep("numeric",784)))
test = read.csv("C:/Users/yiton/Desktop/digit/test.csv", stringsAsFactors=F)

###############
smalltrain=sample(1:nrow(train), 0.8*nrow(train))
newtrain <- train[smalltrain,]
newtest <- train[-smalltrain,]
train.reduced <- newtrain[,apply(newtrain, 2, var, na.rm=TRUE) != 0]

##############pca
pca.result <- prcomp(train[,-1], scale=F, central=F)
summary(pca.result)  #154 comp for 95%
comp <- 87
train.pca <- as.matrix(train[,-1])%*%pca.result$rotation[,1:comp]
dim(train.pca)
label <- train[,1]


#PCA predicts test
test.pca <- as.matrix(test)%*%pca.result$rotation[,1:comp]

##################
svmfit =svm(label~., data=train.pca ,kernel = "linear",cost=.1)
plot(svmfit, train.pca)

cost_grid <- 10^seq(-2, 1, by = 0.25)
tune.out=tune(svm,label~., data=data.frame(train.pca) ,kernel = "linear", ranges = list(cost = cost_grid))
tune.out$best.model

svmpred <- predict(svmfit,test.pca)
out = levels(svmpred)[svmpred] 

write(out,'test4linear.csv')

##############
svmfit2 <- svm(label ~., data=train.pca ,kernel = "polynomial",degree=4, cost = 1)
summary(svmfit2)

svmpred <- predict(svmfit2,test.pca)
head(svmpred)

test$label=svmpred

out = levels(svmpred)[svmpred] 

write(out,'test6poly4.csv')


##################
svmfit3 <- svm(label ~., data=train.pca ,kernel = "radial", gamma =0.2, cost = 10)
summary(svmfit3)

svmpred <- predict(svmfit3,test.pca)
head(svmpred)

test$label=svmpred

out = levels(svmpred)[svmpred] 

write(out,'test7radial.csv')
