set.seed(676)
setwd('/Users/GraceS')
train <- read.csv('/Users/GraceS/GraceShangx Drive/train.csv')
test <- read.csv('/Users/GraceS/GraceShangx Drive/test.csv')

#####Sanity check
#plot the digit
train.d <- train[,-1]
rotate <- function(x) t(apply(x, 2, rev))
m = matrix(unlist(train[10,-1]),nrow = 28,byrow = T)
image(m,col=grey.colors(255))
#reduce zero variance column 
m1 <- m[,apply(m, 2, var, na.rm=TRUE) != 0]
image(m1,col=grey.colors(255))
#reduce zero row
m2 <- m1[apply(m, 1, var, na.rm=TRUE) != 0,]
image(m2,col=grey.colors(255))

 

cat("Start PCA\n") 
#PCA Reduction
pca.result <- prcomp(train[,-1], scale=F, central=F)
summary(pca.result)  #154 comp for 95%
comp <- 87 
train.pca <- as.matrix(train[,-1])%*%pca.result$rotation[,1:comp]
dim(train.pca)
label <- train[,1]

#PCA predicts test
test.pca <- as.matrix(test)%*%pca.result$rotation[1:comp]
########################
cat("Start KNN")
library(class)
library("nnet")
library(stats)

###KNN on the original data
prediction <- knn(train.pca[,1:comp],test.pca[,1:comp],train$label,k=3)
output <- data.frame(ImageId=1:nrow(test),label=prediction) 
write.csv(file='Kaggle-pca.csv', x=output) 
proc.time() 

###KNN WITH PCA REDUCTION
prediction <- knn(train.pca[,1:comp],test.pca[,1:comp],train$label,k=3)
output <- data.frame(ImageId=1:nrow(test),label=prediction)
write.csv(file='Kaggle-pca.csv', x=output)
proc.time()

