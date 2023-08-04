library(rlist)
library("base")
library("tm")
library(caTools)
library(randomForest)
library(caret)
library(pryr)

start_time <- Sys.time() 
dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv") ###This line reads the CSV file "processedDataset.csv" from the specified file path and stores the data in the "dataset" variable.
split <- sample.split(dataset, SplitRatio = 0.7) ### This line uses the "sample.split" function from the "caTools" package to split the dataset into training (70%) and testing (30%) sets. The "split" variable will store the boolean values indicating whether each row belongs to the training set (TRUE) or testing set (FALSE).
### These variables are lists that will store the results of the hyperparameter optimization process.
resultList <- list("accuracy") 
resultList2 <- list("ntree")
resultList3 <- list("mtry")
resultList4 <- list("accuracyOld")
### These lines create a subset of the original dataset containing only the rows that belong to the training set.
train <- subset(dataset, split == "TRUE")
train$y22 <- as.factor(train$y22)  ## y22-> experience
test <- subset(dataset, split == "FALSE")
###proposed method optimization start###
mtry<-c(1:8) ### This line creates a vector "mtry" with values ranging from 1 to 8. You can change it.
j<- mtry[1] ### This line sets the initial value of "j" as the first element of the "mtry" vector (which is 1).
ntree<-c(100:500) ### This line creates a vector "ntree" with values ranging from 100 to 500. You can change it.
j<-ntree[1] ### This line sets the initial value of "j" as the first element of the "ntree" vector (which is 100).
### These variables are used to keep track of accuracy values and the best accuracy achieved during the optimization process.
flag<-0
accuracy<-0.6
accuracyOld<-0.2
max<--1
maxMtry<--1
count<-0
### This line initiates a while loop that will continue until the accuracy reaches 0.60 or the count exceeds 50 (whichever condition is met first). 
###The loop aims to find the optimal hyperparameters within these constraints. Inside the while loop, the code trains a "randomForest" model with the current values of "j" and "mtry", makes predictions on the test set,
###and calculates accuracy. It then compares the accuracy with the best accuracy achieved so far and updates the variables accordingly.
while(accuracy<0.60 || count<=50) ### count is the number of iterations. You can change it.
{
  accuracyOld<-accuracy
  experience = randomForest(y22~., data=train, ntree=j,mtry=8, proximity=T) ### If you want to optimize the mtry value, you should assign the value j to the mtry variable.
  pred1<-predict(experience,test)
  confusionTable<- table(pred1,test$y22)
  accuracy <-sum (confusionTable[1],confusionTable[6],confusionTable[11],confusionTable[16])/ sum (confusionTable[1:16])
  accuracy  
  if(accuracy>=max){
   max<-accuracy
   maxMtry<-j 
  }
  ### resultList and other lists are updated with the accuracy values obtained during each iteration. The loop adjusts the values of "j" (ntree) based on the accuracy improvements. 
  ###The loop continues until the stopping condition is met or the maximum number of iterations is reached.
  resultList <- list.append(resultList,accuracy)
  resultList2 <- list.append(resultList2,500)
  resultList3 <- list.append(resultList3,j)
  resultList4 <- list.append(resultList4,accuracyOld)
  
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
   if(count==50){
    break;
  }
}

resultListTotal <- cbind(resultList4,resultList,resultList2,resultList3) ### This line combines the "resultList4", "resultList", "resultList2", and "resultList3" lists into a single matrix "resultListTotal".
### These variables contain the results of the hyperparameter optimization process, including accuracy values and the best values of "ntree" and "mtry".
resultListTotal
max
maxMtry
### This line calculates the time taken to execute the code by subtracting "start_time" from "end_time".
end_time <- Sys.time()
end_time - start_time
mem_used() ### This function from the "pryr" package is used to calculate the memory used by the R session.

#hyperparameter tuning grid and random search
start_time <- Sys.time()
tunegrid <- expand.grid(.mtry=c(1:8))  ### This line creates a grid of hyperparameters for "mtry" with values ranging from 1 to 8. 
###The "expand.grid" function generates all possible combinations of the specified hyperparameter values.
modellist <- list() ### This line initializes an empty list to store the trained models for different "ntree" values.
### Inside the loop, the train function is used to train a random forest model (method='rf') on the training dataset with the current "ntree" value. 
###The "mtry" hyperparameter is tuned using the "tunegrid" grid, and the evaluation metric used is "Accuracy". The trained model is added to the "modellist" with a unique key based on the current "ntree" value.
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

#Bayesian Optimization
library(MlBayesOpt)
start_time <- Sys.time()
### This line performs the Bayesian optimization for the "randomForest" algorithm. The "rf_opt" function takes several arguments
res0 <- rf_opt(train_data = train,
               train_label = y22,
               test_data = test,
               test_label = y22,
               mtry_range = c(1,8),
               num_tree = c(100,500),.
               init_points = 50,
               n_iter = 1)
end_time <- Sys.time()
end_time - start_time
mem_used()

#neldermead
start_time <- Sys.time()
optimize <-function(mtry){
  dataset <- read.csv ("C:/Users/Desktop/stack/processedDataset.csv")
  split <- sample.split(dataset, SplitRatio = 0.7) 
  train <- subset(dataset, split == "TRUE")  
  train$y22 <- as.factor(train$y22)  
  test <- subset(dataset, split == "FALSE")
  experience = randomForest(y22~., data=train, ntree=100,mtry=mtry, proximity=T)
  pred1<-predict(iris_rf,test)
  confusionTable<- table(pred1,test$y22)
  accuracy <-sum (confusionTable[1],confusionTable[6],confusionTable[11],confusionTable[16])/ sum (confusionTable[1:16])
}
###These lines define the lower and upper bounds for the "mtry" parameter. The optimization will search for the optimal "mtry" value within this range.
library(nloptr)
lower <- 1
upper <- 8
### This line applies the bounded version of the Nelder-Mead optimization algorithm to find the optimal "mtry" value that maximizes the accuracy of the random forest model. 
###The nl.info = TRUE argument enables displaying optimization information.
S <- neldermead(2, optimize, lower, upper, nl.info = TRUE)
end_time <- Sys.time()
end_time - start_time
mem_used()
                     
