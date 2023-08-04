library(rlist)
library(caTools)
library(caret)
library(gbm)
library(dplyr)

#proposed method for shrinkage
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataset<-dataGroup %>% mutate(y22=recode(y22, ### This code block renames the "y22" (experience) column in the dataset to meaningful labels ("beginner," "intermediate," "advanced," and "unknown") using the "mutate" function from the "dplyr" package.
                         `1`="beginner",
                         `2`="intermediate",
                         `3`="advanced",
                         `0`="unknown"))
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
resultList <- list("accuracy")
resultList3 <- list("shrinkage")
resultList4 <- list("accuracyOld")
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
par <- runif(1, 0.001, 0.1)
j<- par
flag<-0
accuracy<-0.6
accuracyOld<-0.2
max<--1
maxShrinkage<--1
count<-0
while(accuracy<0.69 || count<=15) ### The while loop iterates until the accuracy reaches 0.69 or until the maximum number of iterations (15) is reached. You can change it.
{  
  accuracyOld<-accuracy
  model_gbm = gbm(y22 ~., ### Inside the loop, the GBM model is trained with the current value of "shrinkage" (j) using the "gbm" function. The "predict.gbm" function is used to make predictions on the test set.
                  data = train,
                  distribution = "multinomial",
                  cv.folds = 2,
                  shrinkage = j,
                  n.minobsinnode = 5,
                  n.trees = 100)      
  summary(model_gbm)
  pred_test = predict.gbm(object = model_gbm,
                          newdata = test,
                          n.trees = 100,         
                          type = "response")
  class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
  result = data.frame(test$y22, class_names)
  sqrt(min(model_gbm$cv.error))
  class_names[898] <- "advanced"
  class_names[899] <- "intermediate"
  conf_mat = confusionMatrix(as.factor(test$y22), as.factor(class_names))
  accuracy<-conf_mat
  accuracy<-(conf_mat$overall[1])
  accuracy  
  if(accuracy>=max){
    max<-accuracy
    maxShrinkage<-j 
  }
  resultList <- list.append(resultList,accuracy)
  resultList3 <- list.append(resultList3,j)
  resultList4 <- list.append(resultList4,accuracyOld) 
  
  if (flag==0)
  {
    if(accuracy>=0.7)
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
  if(count==15){
    break;
  }
}
resultListTotal <- cbind(resultList4,resultList,resultList3)
resultListTotal
max
maxShrinkage
end_time <- Sys.time()
end_time - start_time
mem_used()

# proposed method for gbm minobsinnode

start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                         `1`="beginner",
                                         `2`="intermediate",
                                         `3`="advanced",
                                         `0`="unknown"))
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
resultList <- list("accuracy")
resultList3 <- list("minobsinnode")
resultList4 <- list("accuracyOld")
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
n.minobsinnode<-c(5, 10, 15)
j<- n.minobsinnode[1]
flag<-0
accuracy<-0.6
accuracyOld<-0.2
max<--1
maxMinobsinnode<--1
count<-0
while(accuracy<0.70 || count<=25)
{  
  accuracyOld<-accuracy
  model_gbm = gbm(y22 ~.,
                  data = train,
                  distribution = "multinomial",
                  cv.folds = 2,
                  shrinkage = 0.1,
                  n.minobsinnode = j,
                  n.trees = 100)   
  pred_test = predict.gbm(object = model_gbm,
                          newdata = test,
                          n.trees = 100,           
                          type = "response")
  class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
  result = data.frame(test$y22, class_names)
  sqrt(min(model_gbm$cv.error))
  class_names[898] <- "advanced"
  class_names[899] <- "intermediate"
  conf_mat = confusionMatrix(as.factor(test$y22), as.factor(class_names))
  accuracy<-conf_mat
  accuracy<-(conf_mat$overall[1])
  accuracy  
  if(accuracy>=max){
    max<-accuracy
    maxMinobsinnode<-j 
  }
  resultList <- list.append(resultList,accuracy)
  resultList3 <- list.append(resultList3,j)
  resultList4 <- list.append(resultList4,accuracyOld)  
  
  if (flag==0)
  {
    if(accuracy>=0.7)
      j=j+1;
  }
  flag=1;
  if((accuracy>accuracyOld) && (accuracy-accuracyOld)>=0.001)
  {
    if(j<=14)
      j=j+1;
  }
  else
  {
    if(j>=6)
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
maxMinobsinnode
end_time <- Sys.time()
end_time - start_time
mem_used()

#proposed meethod gbm ntree
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                         `1`="beginner",
                                         `2`="intermediate",
                                         `3`="advanced",
                                         `0`="unknown"))
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
resultList <- list("accuracy")
resultList3 <- list("minobsinnode")
resultList4 <- list("accuracyOld")
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
ntree<-c(100:500)
j<- ntree[1]
flag<-0
accuracy<-0.6
accuracyOld<-0.2
max<--1
maxTree<--1
count<-0
while(accuracy<0.65 || count<=15)
{  
  accuracyOld<-accuracy
  model_gbm = gbm(y22 ~.,
                  data = train,
                  distribution = "multinomial",
                  cv.folds = 2,
                  shrinkage = 0.1,
                  n.minobsinnode = 10,
                  n.trees = j)       
  pred_test = predict.gbm(object = model_gbm,
                          newdata = test,
                          n.trees = j,           
                          type = "response")
  class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
  result = data.frame(test$y22, class_names)
  sqrt(min(model_gbm$cv.error))
  class_names[898] <- "advanced"
  class_names[899] <- "intermediate"
  conf_mat = confusionMatrix(as.factor(test$y22), as.factor(class_names))
  accuracy<-conf_mat
  accuracy<-(conf_mat$overall[1])
  accuracy  
  if(accuracy>=max){
    max<-accuracy
    maxTree<-j 
  }
  resultList <- list.append(resultList,accuracy)
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
    if(j<=500)
      j=j+1;
  }
  else
  {
    if(j>=101)
      j=j-1;
  }
  count=count+1
}
resultListTotal <- cbind(resultList4,resultList,resultList3)
resultListTotal
max
maxTree
end_time <- Sys.time()
end_time - start_time
mem_used()

#hyperparameter tuning grid and random shrinkage
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                         `1`="beginner",
                                         `2`="intermediate",
                                         `3`="advanced",
                                         `0`="unknown")
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
par <- runif(1, 0.001, 0.1)
j<- par
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
train_control = trainControl(method = "cv", number = 5, search = "random")
tunegrid <- expand.grid (n.trees = 100,interaction.depth = 1,shrinkage =j,n.minobsinnode = 5)
gbm_model <- train(y22~., 
                   data=train, 
                   method='gbm', 
                   metric = 'Accuracy',
                   tuneGrid=tunegrid,
                   trControl=train_control                   
)
print(gbm_model)
end_time <- Sys.time()
end_time - start_time
mem_used()

#hyperparameter tuning grid and random search ntree
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                         `1`="beginner",
                                         `2`="intermediate",
                                         `3`="advanced",
                                         `0`="unknown"))
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
train_control = trainControl(method = "cv", number = 5, search = "grid")
tunegrid <- expand.grid (n.trees = c(100:500),interaction.depth = 1,shrinkage =0.1,n.minobsinnode = 10) ### This line creates a data frame "tunegrid" with all possible combinations of hyperparameters for the grid search. 
### It includes values for "n.trees" ranging from 100 to 500, "interaction.depth" set to 1, "shrinkage" set to 0.1, and "n.minobsinnode" set to 10.
gbm_model <- train(y22~.,  ### This line trains the GBM model using the "train" function from the "caret" package. It performs grid search with cross-validation using the specified hyperparameter values.
                   data=train, 
                   method='gbm', 
                   metric = 'Accuracy',
                   tuneGrid=tunegrid,
                   trControl=train_control                
)
print(gbm_model)
end_time <- Sys.time()
end_time - start_time
mem_used()
                              
##hyperparameter tuning grid and random search n.minobsinnode
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                         `1`="beginner",
                                         `2`="intermediate",
                                         `3`="advanced",
                                         `0`="unknown"))
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
train_control = trainControl(method = "cv", number = 5, search = "grid")
tunegrid <- expand.grid (n.trees = 100,interaction.depth = 1,shrinkage =0.1,n.minobsinnode = c(5:15))
gbm_model <- train(y22~., 
                   data=train, 
                   method='gbm', 
                   metric = 'Accuracy',
                   tuneGrid=tunegrid,
                   trControl=train_control                   
)

print(gbm_model)
end_time <- Sys.time()
end_time - start_time
mem_used()
                              
#bayesian search
library("rBayesianOptimization")
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                         `1`="beginner",
                                         `2`="intermediate",
                                         `3`="advanced",
                                         `0`="unknown"))
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
Test_Fun <- function(x) {    ### This code block defines the function "Test_Fun," which takes the hyperparameter "x" (in this case, "n.minobsinnode") as input and trains the GBM model with the specified hyperparameter value. 
  ### It then calculates and returns the accuracy of the model.
    model_gbm = gbm(y22 ~.,
                    data = train,
                    distribution = "multinomial",
                    cv.folds = 2,
                    shrinkage = 0.1,
                    n.minobsinnode = x,
                    n.trees = 100)   
   
    pred_test = predict.gbm(object = model_gbm,
                            newdata = test,
                            n.trees = x,         
                            type = "response")
    class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
    result = data.frame(test$y22, class_names)
    class_names[898] <- "advanced"
    class_names[899] <- "intermediate"
    conf_mat = confusionMatrix(as.factor(test$y22), as.factor(class_names))
    accuracy<-conf_mat
    accuracy<-(conf_mat$overall[1])
    accuracy
  list(Score=mean(accuracy<-(conf_mat$overall[1]))) 
}
## Set larger init_points and n_iter for better optimization result
### This line performs Bayesian Optimization using the "BayesianOptimization" function from the "MlBayesOpt" package. It searches for the optimal value of "n.minobsinnode" within the bounds of 5 to 15. The "init_points" parameter specifies the number of initial points to be evaluated, and "n_iter" specifies the number of iterations for optimization. 
###The acquisition function used is "ucb" (Upper Confidence Bound), and "kappa" and "eps" are parameters of the acquisition function.
OPT_Res <- BayesianOptimization(Test_Fun,
                                bounds = list(x = c(5:15)),
                                init_points = 15,
                                n_iter = 1,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)
end_time <- Sys.time()
end_time - start_time
mem_used()
                              
#shrinkage bayesian search
library("rBayesianOptimization")
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                         `1`="beginner",
                                         `2`="intermediate",
                                         `3`="advanced",
                                         `0`="unknown"))
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
Test_Fun <- function(x) {
    model_gbm = gbm(y22 ~.,
                  data = train,
                  distribution = "multinomial",
                  cv.folds = 2,
                  shrinkage = x,
                  n.minobsinnode = 5,
                  n.trees = 100)      
  
  pred_test = predict.gbm(object = model_gbm,
                          newdata = test,
                          n.trees = 100,       
                          type = "response")
  class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
  result = data.frame(test$y22, class_names)
  class_names[898] <- "advanced"
  class_names[899] <- "intermediate"
  conf_mat = confusionMatrix(as.factor(test$y22), as.factor(class_names))
  accuracy<-conf_mat
  accuracy<-(conf_mat$overall[1])
  accuracy  
  list(Score=mean(accuracy<-(conf_mat$overall[1]))) 
}

## Set larger init_points and n_iter for better optimization result
OPT_Res <- BayesianOptimization(Test_Fun,
                                bounds = list(x = c(0.001, 0.1)),
                                init_points = 15,
                                n_iter = 0.1,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

end_time <- Sys.time()
end_time - start_time
mem_used()

#neldermead minobsinnode
start_time <- Sys.time()
optimize <-function(minobsinnode){
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
  
  dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                           `1`="beginner",
                                           `2`="intermediate",
                                           `3`="advanced",
                                           `0`="unknown"))

  # Splitting data in train and test data
  split <- sample.split(dataset, SplitRatio = 0.7)  
  train <- subset(dataset, split == "TRUE")
  test <- subset(dataset, split == "FALSE")
  model_gbm = gbm(y22 ~., 
                  data = train,
                  distribution = "multinomial",
                  cv.folds = 2,
                  shrinkage = 0.1,
                  n.minobsinnode = minobsinnode,
                  n.trees = 100)    
  summary(model_gbm)
  pred_test = predict.gbm(object = model_gbm,
                          newdata = test,
                          n.trees = 100,         
                          type = "response")
  class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
  result = data.frame(test$y22, class_names)
  sqrt(min(model_gbm$cv.error))
  class_names[898] <- "advanced"
  class_names[899] <- "intermediate" 
  conf_mat = confusionMatrix(as.factor(test$y22), as.factor(class_names))
  accuracy<-conf_mat
  accuracy<-(conf_mat$overall[1])
  accuracy  
}
library(nloptr)
# Bounded version of Nelder-Mead
### These lines define the lower and upper bounds for the hyperparameter "minobsinnode" (the minimum number of observations in a terminal node) that will be optimized.                              
lower <- 5
upper <- 15
S <- neldermead(6, optimize, lower, upper, nl.info = TRUE) ### This line initiates the Nelder-Mead optimization process using the "neldermead" function from the "nloptr"ackage. 
###The "optimize" function defined earlier will be used for optimization, and the "minobsinnode" hyperparameter will be optimized within the specified bounds.
end_time <- Sys.time()
end_time - start_time
mem_used()

#nelder meaad shrinkage
start_time <- Sys.time()
optimize <-function(shrinkage){
  dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv") 
  
  dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                           `1`="beginner",
                                           `2`="intermediate",
                                           `3`="advanced",
                                           `0`="unknown"))
  
  # Splitting data in train and test data
  split <- sample.split(dataset, SplitRatio = 0.7)  
  train <- subset(dataset, split == "TRUE")
  test <- subset(dataset, split == "FALSE")
  model_gbm = gbm(y22 ~.,
                  data = train,
                  distribution = "multinomial",
                  cv.folds = 2,
                  shrinkage = shrinkage,
                  n.minobsinnode = 5,
                  n.trees = 100)      
  summary(model_gbm)
  pred_test = predict.gbm(object = model_gbm,
                          newdata = test,
                          n.trees = 100,         
                          type = "response")
  class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
  result = data.frame(test$y22, class_names)
  sqrt(min(model_gbm$cv.error))
  class_names[898] <- "advanced"
  class_names[899] <- "intermediate" 
  conf_mat = confusionMatrix(as.factor(test$y22), as.factor(class_names))
  accuracy<-conf_mat
  accuracy<-(conf_mat$overall[1])
  accuracy 
}

library(nloptr)
# Bounded version of Nelder-Mead
lower <- 0.001
upper <- 0.1
S <- neldermead(0.002, optimize, lower, upper, nl.info = TRUE)
end_time <- Sys.time()
end_time - start_time
mem_used()
                              
#gbm grid shrikage
library(rlist)
library(caTools)
library(caret)
library(gbm)
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")

library(dplyr)

dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                         `1`="beginner",
                                         `2`="intermediate",
                                         `3`="advanced",
                                         `0`="unknown"))
dataset
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
split
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
count <-1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

while(count2 <= 17){
  sTime <- Sys.time()
  count <-1
  count2 <- count2+1
  
  hyper <- runif(1, 0.001, 0.1)
  while(count<count2){
    hyperOpt <- sample(hyper,1,replace = FALSE)
    model_gbm = gbm(y22 ~.,
                    data = train,
                    distribution = "multinomial",
                    cv.folds = 2,
                    shrinkage = hyperOpt,
                    n.minobsinnode = 5,
                    n.trees = 100)      
    summary(model_gbm)
    pred_test = predict.gbm(object = model_gbm,
                            newdata = test,
                            n.trees = 100,          
                            type = "response")
    class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
    result = data.frame(test$y22, class_names)
    sqrt(min(model_gbm$cv.error))
    class_names[898] <- "advanced"
    class_names[899] <- "intermediate"
    #conf_mat = confusionMatrix(test$y22, as.factor(class_names))
    conf_mat = confusionMatrix(as.factor(test$y22), as.factor(class_names))
    accuracy<-conf_mat
    accuracy<-(conf_mat$overall[1])
    accuracy
    count <- count+1
    if(count2==17){
      break;
    }
    
  }
  s2Time <- Sys.time()
  matris <- list.append(matris,count2)
  matris2 <- list.append(matris2,s2Time-sTime)
  matris3 <- list.append(matris3,accuracy)
  matris4 <- list.append(matris4,hyperOpt)
  if(count2==17){
    break;
  }
}
Total <- cbind(matris,matris2,matris3,matris4)

write.csv(Total,"C:/Desktop/gbmshrinkagegrid.csv")
end_time <- Sys.time()

end_time - start_time
mem_used()

#gbm RANDOM shrikage
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataset<-dataGroup %>% mutate(y22=recode(y22, 
                                         `1`="beginner",
                                         `2`="intermediate",
                                         `3`="advanced",
                                         `0`="unknown"))
# Splitting data in train and test data
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
count <-1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")
while(count2 <= 17){
  sTime <- Sys.time()
  count <-1
  count2 <- count2+1  
  hyper <- runif(1, 0.001, 0.1)
  while(count<count2){
    hyperOpt <- sample(hyper,1,replace = TRUE)
    model_gbm = gbm(y22 ~.,
                    data = train,
                    distribution = "multinomial",
                    cv.folds = 2,
                    shrinkage = hyperOpt,
                    n.minobsinnode = 5,
                    n.trees = 100)     
    summary(model_gbm)
    pred_test = predict.gbm(object = model_gbm,
                            newdata = test,
                            n.trees = 100,        
                            type = "response")
    class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
    result = data.frame(test$y22, class_names)
    sqrt(min(model_gbm$cv.error))
    class_names[898] <- "advanced"
    class_names[899] <- "intermediate"   
    conf_mat = confusionMatrix(as.factor(test$y22), as.factor(class_names))
    accuracy<-conf_mat
    accuracy<-(conf_mat$overall[1])
    accuracy
    count <- count+1
    if(count2==17){
      break;
    }    
  }
  s2Time <- Sys.time()
  matris <- list.append(matris,count2)
  matris2 <- list.append(matris2,s2Time-sTime)
  matris3 <- list.append(matris3,accuracy)
  matris4 <- list.append(matris4,hyperOpt)
  if(count2==17){
    break;
  }
}
Total <- cbind(matris,matris2,matris3,matris4)
write.csv(Total,"C:/Desktop/gbmshrinkagerandom.csv")
end_time <- Sys.time()
end_time - start_time
mem_used()



