library(rlist)
library("base")
library("tm")
library(caTools)
library(randomForest)
library(caret)
library(pryr)

start_time <- Sys.time()

dataset <- read.csv ("C:/Users/fatmaaltinsoy/Desktop/stack/islenmisDosya3.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
split
sonucList <- list("accuracy")
sonucList2 <- list("ntree")
sonucList3 <- list("mtry")
sonucList4 <- list("accuracyOld")
train <- subset(dataset, split == "TRUE")
train
train$y22 <- as.factor(train$y22)
train$y22
test <- subset(dataset, split == "FALSE")
mtry<-c(1:8)
mtry
j<- mtry[1]
j
ntree<-c(100:500)
ntree
j<-ntree[1]
flag<-0
accuracy<-0.6
accuracyOld<-0.2
enBuyuk<--1
enBuyukMtry<--1

count<-0
while(accuracy<0.60 || count<=50)
{
  accuracyOld<-accuracy
  iris_rf = randomForest(y22~., data=train, ntree=j,mtry=8, proximity=T) #ntree 500, mtry=8 default değerler
  pred1<-predict(iris_rf,test)
  confusionTable<- table(pred1,test$y22)
  confusionTable 
  accuracy <-sum (confusionTable[1],confusionTable[6],confusionTable[11],confusionTable[16])/ sum (confusionTable[1:16])
  accuracy
  # confusionTable<- data.frame(pred1,test$y22)
  # roundedresults<-sapply(pred1,round,digits=0)
  # roundedresults
  # roundedresultsdf=data.frame(roundedresults,test$y22)
  # roundedresultsdf
  # attach(roundedresultsdf)
  # roundedresultsdf
  # example <- confusionMatrix(data=roundedresults, reference = test$y22)  
  
 
  if(accuracy>=enBuyuk){
    enBuyuk<-accuracy
    enBuyukMtry<-j 
  }
  sonucList <- list.append(sonucList,accuracy)
  sonucList2 <- list.append(sonucList2,500)
  sonucList3 <- list.append(sonucList3,j)
  sonucList4 <- list.append(sonucList4,accuracyOld)
  
  if (flag==0)
  {
    if(accuracy>=0.6)
      j=j+1;
  }
  flag=1;
  if((accuracy>accuracyOld) && (accuracy-accuracyOld)>=0.001)
  {
    if(j<500)
      j=j+100;
  }
  else
  {
    if(j>=200)
      j=j-100;
  }
  count=count+1
}

sonucListGenel <- cbind(sonucList4,sonucList,sonucList2,sonucList3)
sonucListGenel
enBuyuk
enBuyukMtry
end_time <- Sys.time()
end_time - start_time
mem_used()

#hyperparameter tuning grid and random
library(rlist)
library("base")
library("tm")
library("utiml")
library(caTools)
library(randomForest)
library(MASS)
library(caret)
library(pryr)

start_time <- Sys.time()

dataset <- read.csv ("C:/Users/fatmaaltinsoy/Desktop/stack/islenmisDosya3.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
split
train <- subset(dataset, split == "TRUE")
train
train$y22 <- as.factor(train$y22)
test <- subset(dataset, split == "FALSE")
train_control = trainControl(method = "cv", number = 5, search = "grid")
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=c(1:8))
modellist <- list()
for (ntree in c(100,200,300,400,500)){
  rf_default <- train(y22~., 
                      data=train, 
                      method='rf', 
                      metric = 'Accuracy',
                      tuneGrid=tunegrid,
                      trControl=train_control,
                      ntree=ntree
                      
  )
  key <- toString(ntree)
  modellist[[key]] <- rf_default
}
results <- resamples(modellist)
summary(results)
print(rf_default)
end_time <- Sys.time()
end_time - start_time
mem_used()
pred_test <- predict(rf_default, test)
pred_test
confusionMatrix(table(pred_test,test$y22))
dotplot(results)


# bestmtry <- tuneRF(train,train$y22 ,stepFactor = 1.2, improve = 0.01, trace=T, plot= T) 
# model <- randomForest(y22~.,data= train)
# model
# pred_test <- predict(model, newdata = test, type= "class")
# pred_test
# confusionMatrix(table(pred_test,test$y22))


#Bayesian Optimization
library(MlBayesOpt)
start_time <- Sys.time()

dataset <- read.csv ("C:/Users/fatmaaltinsoy/Desktop/stack/islenmisDosya3.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
split
train <- subset(dataset, split == "TRUE")
train
train$y22 <- as.factor(train$y22)
test <- subset(dataset, split == "FALSE")
test$y22<- as.factor(test$y22)

res0 <- rf_opt(train_data = train,
               train_label = y22,
               test_data = test,
               test_label = y22,
               mtry_range = c(1,8),
               num_tree = c(100,500),
               init_points = 50,
               n_iter = 1)
end_time <- Sys.time()

end_time - start_time
mem_used()

#neldermead
library(rlist)
library("base")
library("tm")
library("utiml")
library(rlist)
library(caTools)
library(randomForest)
library(MASS)
library(caret)
start_time <- Sys.time()
optimize <-function(mtry){
  dataset <- read.csv ("C:/Users/fatmaaltinsoy/Desktop/stack/islenmisDosya3.csv")
  split <- sample.split(dataset, SplitRatio = 0.7)
  split
  train <- subset(dataset, split == "TRUE")
  train
  train$y22 <- as.factor(train$y22)
  train$y22
  test <- subset(dataset, split == "FALSE")
  iris_rf = randomForest(y22~., data=train, ntree=100,mtry=mtry, proximity=T) #ntree 500, mtry=8 default değerler
  pred1<-predict(iris_rf,test)
  confusionTable<- table(pred1,test$y22)
  confusionTable 
  accuracy <-sum (confusionTable[1],confusionTable[6],confusionTable[11],confusionTable[16])/ sum (confusionTable[1:16])
  accuracy
}


library(nloptr)
# Bounded version of Nelder-Mead
lower <- 1
upper <- 8
S <- neldermead(2, optimize, lower, upper, nl.info = TRUE)
# $xmin = c(0.7085595, 0.5000000, 0.2500000)
# $fmin = 0.3353605
## End(Not run)


end_time <- Sys.time()

end_time - start_time
mem_used()

#neldermead ntree
library(rlist)
library("base")
library("tm")
library("utiml")
library(rlist)
library(caTools)
library(randomForest)
library(MASS)
library(caret)
start_time <- Sys.time()
optimize <-function(ntree){
  dataset <- read.csv ("C:/Users/fatmaaltinsoy/Desktop/stack/islenmisDosya3.csv")
  split <- sample.split(dataset, SplitRatio = 0.7)
  split
  train <- subset(dataset, split == "TRUE")
  train
  train$y22 <- as.factor(train$y22)
  train$y22
  test <- subset(dataset, split == "FALSE")
  iris_rf = randomForest(y22~., data=train, ntree=ntree) #ntree 500, mtry=8 default değerler
  pred1<-predict(iris_rf,test)
  confusionTable<- table(pred1,test$y22)
  confusionTable 
  accuracy <-sum (confusionTable[1],confusionTable[6],confusionTable[11],confusionTable[16])/ sum (confusionTable[1:16])
  accuracy
}


library(nloptr)
# Bounded version of Nelder-Mead
lower <- 100
upper <- 200
S <- neldermead(101, optimize, lower, upper, nl.info = TRUE)
# $xmin = c(0.7085595, 0.5000000, 0.2500000)
# $fmin = 0.3353605
## End(Not run)


end_time <- Sys.time()

end_time - start_time
mem_used()

                     
