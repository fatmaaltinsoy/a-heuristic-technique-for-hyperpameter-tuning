library("e1071")
library(caret)
library(rlist)
library("base")
library("tm")
library(caTools)
library(pryr)
#proposed method for svm degree
start_time <- Sys.time()
dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
degree=c(1:10)
j<- degree[1]
flag<-0
accuracy<-0.6
accuracyOld<-0.2
max<--1
maxcost<--1
resultList <- list("accuracy")
resultList3 <- list("degree")
resultList4 <- list("accuracyOld")
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
count<-0
while(accuracy<0.70 || count<=25)
{
svm_model <- svm(y22 ~ ., data=train, type = 'C-classification',kernel = 'polynomial',degree=j)
pred = predict(svm_model,test)
cm = table(test$y22, pred)
accuracy <-sum (cm[1],cm[6],cm[11],cm[16])/ sum (cm[1:16])
accuracy
if(accuracy>=max){
  max<-accuracy
  maxdegree<-j 
}
resultList <- list.append(resultList,accuracy)
resultList3 <- list.append(resultList3,j)
resultList4 <- list.append(resultList4,accuracyOld)
if (flag==0)
{
  if(accuracy>=0.65)
    j=j+1;
}
flag=1;
if((accuracy>accuracyOld) && (accuracy-accuracyOld)>=0.001)
{
  if(j<=9)
    j=j+1;
}
else
{
  if(j>=2)
    j=j-1;
}
count=count+1
if(count==25){
  break;
}
}
resultListTotal <- cbind(resultList4,resultList,resultList3)
resultListTotal
max
maxdegree
end_time <- Sys.time()
end_time - start_time
mem_used()

#svm hyperparameter tuning random and grid search for cost

start_time <- Sys.time()
dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
train$y22 <- as.factor(train$y22)
train_control = trainControl(method = "cv", number = 5, search = "grid")
tunegrid <- expand.grid (degree = 3, scale = 1, C = c(1:10))
svm_model <- train(y22~., 
                      data=train, 
                      method='svmPoly', 
                      metric = 'Accuracy',
                      tuneGrid=tunegrid,
                      trControl=train_control
                     )
print(svm_model)
end_time <- Sys.time()
end_time - start_time
mem_used()

#svm hyperparameter tuning bayesian
library(MlBayesOpt)
start_time <- Sys.time()
dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
train$y22 <- as.factor(train$y22)
test <- subset(dataset, split == "FALSE")
test$y22<- as.factor(test$y22)
res0 <- svm_opt(train_data = train,
               train_label = y22,
               test_data = test,
               test_label = y22,
               svm_kernel="sigmoid",
               init_points = 10,
               n_iter = 1)
end_time <- Sys.time()
end_time - start_time
mem_used()

#svm hyperparameter tuning random and grid search gamma

start_time <- Sys.time()
dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
train$y22 <- as.factor(train$y22)
train_control = trainControl(method = "cv", number = 5, search = "random")
tunegrid <- expand.grid (gamma = c(0.001,0.1), scale = 1, C = 1)
svm_model <- train(y22~., 
                   data=train, 
                   method='svmPoly', 
                   metric = 'Accuracy',
                   tuneGrid=tunegrid,
                   trControl=train_control
)
print(svm_model)
end_time <- Sys.time()
end_time - start_time
mem_used()

#neldermead cost

start_time <- Sys.time()
optimize <-function(cost){
  dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
  split <- sample.split(dataset, SplitRatio = 0.7) 
  train <- subset(dataset, split == "TRUE")
  test <- subset(dataset, split == "FALSE")
  svm_model <- svm(y22 ~ ., data=train, type = 'C-classification',kernel = 'polynomial',cost=cost)
  pred = predict(svm_model,test)
  cm = table(test$y22, pred) 
  accuracy <-sum (cm[1],cm[6],cm[11],cm[16])/ sum (cm[1:16])
  accuracy  
}
library(nloptr)
# Bounded version of Nelder-Mead
lower <- 1
upper <- 10
S <- neldermead(2, optimize, lower, upper, nl.info = TRUE)
end_time <- Sys.time()
end_time - start_time
mem_used()

#neldermead gamma
start_time <- Sys.time()
optimize <-function(gamma){
  dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
  split <- sample.split(dataset, SplitRatio = 0.7)
  train <- subset(dataset, split == "TRUE")
  test <- subset(dataset, split == "FALSE")
  svm_model <- svm(y22 ~ ., data=train, type = 'C-classification',kernel = 'polynomial',gamma=gamma)
  pred = predict(svm_model,test)
  cm = table(test$y22, pred)  
  accuracy <-sum (cm[1],cm[6],cm[11],cm[16])/ sum (cm[1:16])
  accuracy
  
}
library(nloptr)
# Bounded version of Nelder-Mead
lower <- 0.001
upper <- 0.01
S <- neldermead(0.002, optimize, lower, upper, nl.info = TRUE)
end_time <- Sys.time()
end_time - start_time
mem_used()

#proposed method svm gamma

start_time <- Sys.time()
dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
par <- runif(1, 0.001, 0.1)
j<- par
flag<-0
accuracy<-0.6
accuracyOld<-0.2
max<--1
maxgamma<--1
resultList <- list("accuracy")
resultList2 <- list("ntree")
resultList3 <- list("gamma")
resultList4 <- list("accuracyOld")
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
count<-0
while(accuracy<0.55 || count<=25)
{
  svm_model <- svm(y22 ~ ., data=train, type = 'C-classification',kernel = 'polynomial', gamma=j)
  pred = predict(svm_model,test)
  cm = table(test$y22, pred)
  accuracy <-sum (cm[1],cm[6],cm[11],cm[16])/ sum (cm[1:16])
  accuracy  
  if(accuracy>=max){
    max<-accuracy
    maxgamma<-j 
  }
  resultList <- list.append(resultList,accuracy)
  resultList3 <- list.append(resultList3,j)
  resultList4 <- list.append(resultList4,accuracyOld)
  if (flag==0)
  {
    if(accuracy>=0.55)
      j=j+0.1;
  }
  flag=1;
  if((accuracy>accuracyOld) && (accuracy-accuracyOld)>=0.001)
  {
    if(j<=0.9)
      j=j+0.1;
  }
  else
  {
    if(j>=0.002)
      j=j-0.1;
  }
  count=count+1
  if(count==25){
    break;
  }
}
resultListTotal <- cbind(resultList4,resultList,resultList3)
resultListTotal
max
maxgamma
end_time <- Sys.time()
end_time - start_time
mem_used()

#proposed method svm cost

start_time <- Sys.time()
dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
par <- runif(1, 0.001, 0.1)
j<- par
flag<-0
accuracy<-0.6
accuracyOld<-0.2
max<--1
maxcost<--1
resultList <- list("accuracy")
resultList3 <- list("cost")
resultList4 <- list("accuracyOld")
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
count<-0
while(accuracy<0.80 || count<=25)
{
  svm_model <- svm(y22 ~ ., data=train, type = 'C-classification',kernel = 'polynomial', cost=j)
  pred = predict(svm_model,test)
  cm = table(test$y22, pred)
  accuracy <-sum (cm[1],cm[6],cm[11],cm[16])/ sum (cm[1:16])
  accuracy  
  if(accuracy>=max){
    max<-accuracy
    maxcost<-j 
  }
  resultList <- list.append(resultList,accuracy)
  resultList3 <- list.append(resultList3,j)
  resultList4 <- list.append(resultList4,accuracyOld)
  if (flag==0)
  {
    if(accuracy>=0.55)
      j=j+1;
  }
  flag=1;
  if((accuracy>accuracyOld) && (accuracy-accuracyOld)>=0.001)
  {
   if(j<=0.9)
      j=j+0.1;
  }
  else
  {
    if(j>=0.002)
      j=j-0.1;
  }
  count=count+1
  if(count==25){
    break;
  }
}
resultListTotal <- cbind(resultList4,resultList,resultList3)
resultListTotal
end_time <- Sys.time()
end_time - start_time
mem_used()

#SVM GAMMA GRID SEARCH

count <-1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")
dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
start_time <- Sys.time()
while(count2 <= 27){
  sTime <- Sys.time()
  count <-1
  count2 <- count2+1  
  hyper <- runif(1, 0.001, 0.1)
  while(count<count2){
    hyperOpt <- sample(hyper,1,replace = FALSE)
    svm_model <- svm(y22 ~ ., data=train, type = 'C-classification',kernel = 'polynomial', gamma=hyperOpt)
    pred = predict(svm_model,test)
    cm = table(test$y22, pred)  
    accuracy <-sum (cm[1],cm[6],cm[11],cm[16])/ sum (cm[1:16])
    accuracy
    count <- count+1
    if(count2==27){
      break;
    }
    
  }
  s2Time <- Sys.time()
  matris <- list.append(matris,count2)
  matris2 <- list.append(matris2,s2Time-sTime)
  matris3 <- list.append(matris3,accuracy)
  matris4 <- list.append(matris4,hyperOpt)
  if(count2==27){
    break;
  }
}
Total <- cbind(matris,matris2,matris3,matris4)
write.csv(Total,"C:/Desktop/svmgridgamma.csv")
end_time <- Sys.time()
end_time - start_time
mem_used()

#SVM GAMMA RANDOM SEARCH

count <-1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")
dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
start_time <- Sys.time()
while(count2 <= 27){
  sTime <- Sys.time()
  count <-1
  count2 <- count2+1  
  hyper <- runif(1, 0.001, 0.1)
  while(count<count2){
    hyperOpt <- sample(hyper,1,replace = TRUE)
    svm_model <- svm(y22 ~ ., data=train, type = 'C-classification',kernel = 'polynomial', gamma=hyperOpt)
    pred = predict(svm_model,test)
    cm = table(test$y22, pred)
    accuracy <-sum (cm[1],cm[6],cm[11],cm[16])/ sum (cm[1:16])
    accuracy
    count <- count+1
    if(count2==27){
      break;
    }    
  }
  s2Time <- Sys.time()
  matris <- list.append(matris,count2)
  matris2 <- list.append(matris2,s2Time-sTime)
  matris3 <- list.append(matris3,accuracy)
  matris4 <- list.append(matris4,hyperOpt)
  if(count2==27){
    break;
  }
}
Total <- cbind(matris,matris2,matris3,matris4)
write.csv(Total,"C:/Desktop/svmrandomgamma.csv")
end_time <- Sys.time()
end_time - start_time
mem_used()
