# 필요한 경우 라이브러리를 다운받는다
#install.packages("rpart")
#install.packages("e1071")
#install.packages("neuralnet")
#install.packages("ggplot2")
#install.packages("devtools")
#library(devtools)
#install_github("ggbiplot", "vqv")
#install.packages("randomForest")
#install.packages("gbm")

# 필요한 라이브러리를 불러온다
library(ggplot2) # plotting functions
library(ggbiplot)
library(rpart) # CART library
library(e1071) # SVM library
library(neuralnet) # Neural Network library
library(randomForest)
library(gbm)
library(class) # knn
library(MASS) # lda

# 데이터를 준비한다
DATA <- read.table('data_iris.txt',header=TRUE)
DATA[1:10,] # 데이터를 확인한다

# Exploratory Data Analysis
summary(DATA)
pairs(DATA) # scatter plot of all pairs
is.na(DATA) # 결손치 체크
colSums(is.na(DATA)) # 각 변수별 결손치의 갯수
Y <- DATA[,5] # Species는 종속변수
X <- DATA[,1:4] # 나머지 변수가 독립변수
pairs(DATA,col=Y)

##########################################################
# Unsupervied Learning
##########################################################

##########################################################
# PCA
##########################################################

pr <- prcomp(X) # Principal component calculation
plot(pr) # explained variance
ggbiplot(pr,group=Y) # biplot

###########################################################
# k-mean clustering
###########################################################

g <- kmeans(X,centers=3,nstart=100) # kmeans clustering calculation
s1 <- table(Y,g$cluster) # checking clusters vs. true categories
pairs(DATA,col=Y,pch=g$cluster) # visualization

g <- kmeans(pr$x[,1],centers=3,nstart=100)
s2 <- table(Y,g$cluster) # checking clusters vs. true categories
pairs(DATA,col=Y,pch=g$cluster) # visualization

s1 # 16 miss-clasified
s2 # 13 miss-clasified




###########################################################
# Supervied Learning (Regression)
###########################################################

# problem setting
train.idx <- c(1:30,51:80,91:120)
data.train <- DATA[train.idx,1:4]
data.test <- DATA[-train.idx,1:4]

###########################################################
# linear regression
###########################################################

# model fitting
f.lm <- lm(Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,data=data.train)

# predict with the model
y.train.lm <- predict(f.lm,data.train)
y.test.lm <- predict(f.lm,data.test)

# measure rmses
rmse.train.lm <- sqrt(mean((data.train$Petal.Width-y.train.lm)^2))
rmse.test.lm <- sqrt(mean((data.test$Petal.Width-y.test.lm)^2))

# plotting: estimated vs. measured
plot(y.train.lm,data.train$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)
plot(y.test.lm,data.test$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)

###########################################################
# regression tree
###########################################################

# model fitting
f.rt <- rpart(Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,data=data.train)
plot(f.rt)
text(f.rt,use.n=TRUE)

# predict with the model
y.train.rt <- predict(f.rt,data.train)
y.test.rt <- predict(f.rt,data.test)

# measure rmses
rmse.train.rt <- sqrt(mean((data.train$Petal.Width-y.train.rt)^2))
rmse.test.rt <- sqrt(mean((data.test$Petal.Width-y.test.rt)^2))

# plotting: estimated vs. measured
plot(y.train.rt,data.train$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)
plot(y.test.rt,data.test$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)

###########################################################
# random forest
###########################################################

f.rf <- randomForest(Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,data=data.train)
plot(f.rf)

# predict with the model
y.train.rf <- predict(f.rf,data.train)
y.test.rf <- predict(f.rf,data.test)

# measure rmses
rmse.train.rf <- sqrt(mean((data.train$Petal.Width-y.train.rf)^2))
rmse.test.rf <- sqrt(mean((data.test$Petal.Width-y.test.rf)^2))

# plotting: estimated vs. measured
plot(y.train.rf,data.train$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)
plot(y.test.rf,data.test$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)


###########################################################
# boosted tree
###########################################################

f.gbm <- gbm(Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,data=data.train,n.trees=1000,shrinkage=0.05)
plot(f.gbm)

# predict with the model
y.train.gbm <- predict(f.gbm,data.train,n.trees=1000)
y.test.gbm <- predict(f.gbm,data.test,n.trees=1000)

# measure rmses
rmse.train.gbm <- sqrt(mean((data.train$Petal.Width-y.train.gbm)^2))
rmse.test.gbm <- sqrt(mean((data.test$Petal.Width-y.test.gbm)^2))

# plotting: estimated vs. measured
plot(y.train.gbm,data.train$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)
plot(y.test.gbm,data.test$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)


###########################################################
# neural network
###########################################################

f.nn <- neuralnet(Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,data=data.train,hidden=c(4,4))
plot(f.nn)

# predict with the model
y.train.nn <- compute(f.nn,data.train[,1:3])$net.result
y.test.nn <- compute(f.nn,data.test[,1:3])$net.result

# measure rmses
rmse.train.nn <- sqrt(mean((data.train$Petal.Width-y.train.nn)^2))
rmse.test.nn <- sqrt(mean((data.test$Petal.Width-y.test.nn)^2))

# plotting: estimated vs. measured
plot(y.train.nn,data.train$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)
plot(y.test.nn,data.test$Petal.Width,ylim=c(0,2.5),xlim=c(0,2.5))
lines(c(-10,10),c(-10,10),col=2)


###########################################################
# Summary
###########################################################

ERR = matrix(c(rmse.train.lm,rmse.train.rt,rmse.train.rf,rmse.train.gbm,rmse.train.nn,
               rmse.test.lm,rmse.test.rt,rmse.test.rf,rmse.test.gbm,rmse.test.nn), ncol=2)
row.names(ERR) = c("lm","CART","RF","GBM","NN");
colnames(ERR) = c("Train","Test")
ERR = t(ERR)

barplot(ERR,beside=TRUE,legend.text=c("Train","Test"))

# DO IT YOUSELF
# Please reconfigure the NN structure for better performance. What is your test error?



###########################################################
# Supervied Learning (Classification)
###########################################################

# problem setting
train.idx <- c(1:30,51:80,91:120)
data.train <- DATA[train.idx,1:5]
data.test <- DATA[-train.idx,1:5]


###########################################################
# knn
###########################################################

# predict
y.train.knn <- knn(data.train[,1:4],data.train[,1:4],data.train[,5],k=5)
y.test.knn <- knn(data.train[,1:4],data.test[,1:4],data.train[,5],k=5)

# measure errors
err.train.knn <- sum(y.train.knn!=data.train$Species)/nrow(data.train)
err.test.knn <- sum(y.test.knn!=data.test$Species)/nrow(data.test)

# cases
table(y.test.knn,data.test$Species)
table(y.train.knn,data.train$Species)

# What is k for the least test error?
# what is k for the least train error?


###########################################################
# lda
###########################################################

f.lda <- lda(Species~.,data=data.train)

# predict with the model
y.train.lda <- predict(f.lda,data.train)
y.test.lda <- predict(f.lda,data.test)

# measure errors
err.train.lda <- sum(y.train.lda$class!=data.train$Species)/nrow(data.train)
err.test.lda <- sum(y.test.lda$class!=data.test$Species)/nrow(data.test)

# cases
table(y.test.lda$class,data.test$Species)
table(y.train.lda$class,data.train$Species)


###########################################################
# svm
###########################################################

f.svm <- svm(Species~.,data=data.train)

# predict with the model
y.train.svm <- predict(f.svm,data.train)
y.test.svm <- predict(f.svm,data.test)

# measure errors
err.train.svm <- sum(y.train.svm!=data.train$Species)/nrow(data.train)
err.test.svm <- sum(y.test.svm!=data.test$Species)/nrow(data.test)

# cases
table(y.test.svm,data.test$Species)
table(y.train.svm,data.train$Species)


###########################################################
# neural network
###########################################################

# encode data
Y.DATA <- cbind( c(rep(1,50),rep(0,100)), # index for setosa
                 c(rep(0,50),rep(1,50),rep(0,50)), # index for versicolor
                 c(rep(0,100),rep(1,50)) ) # index for virginica
colnames(Y.DATA) = c('setosa','versicolor','virginica')
data.train.nn <- cbind(data.train[,1:4],Y.DATA[train.idx,])

# fit the model
f.nn <- neuralnet(setosa+versicolor+virginica ~ 
          Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
          data=data.train.nn,hidden=c(4,4))

# predict with the model
y.train.nn.org <- compute(f.nn,data.train[,1:4])
y.train.nn = apply(y.train.nn.org$net.result,1,which.max)
y.train.nn = factor(y.train.nn,labels=colnames(Y.DATA))

y.test.nn.org <- compute(f.nn,data.test[,1:4])
y.test.nn = apply(y.test.nn.org$net.result,1,which.max)
y.test.nn = factor(y.test.nn,labels=colnames(Y.DATA))

# measure errors
err.train.nn <- sum(y.train.nn!=data.train$Species)/nrow(data.train)
err.test.nn <- sum(y.test.nn!=data.test$Species)/nrow(data.test)

# cases
table(y.test.nn,data.test$Species)
table(y.train.nn,data.train$Species)


###########################################################
# Summary
###########################################################

ERR = matrix(c(err.train.knn,err.train.lda,err.train.svm,err.train.nn,
               err.test.knn,err.test.lda,err.test.svm,err.test.nn), ncol=2)
row.names(ERR) = c("KNN","LDA","SVM","NN");
colnames(ERR) = c("Train","Test")
ERR = t(ERR)

barplot(ERR,beside=TRUE,legend.text=c("Train","Test"))

# DO IT YOUSELF
# Please reconfigure the NN structure for better performance. What is your test error?


