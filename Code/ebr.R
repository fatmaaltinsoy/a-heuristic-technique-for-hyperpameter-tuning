library("utiml")
library("mldr")
library("parallel")
library("ROCR")
#proposed method ebr attr.space
start_time <- Sys.time()
###get 100 random instances
dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
dataGroup <- dataGroup[,-1]
###this block gets specific columns of the features
fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") ### c(51:72) values between y1-y23
class(fileM)
ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
resultList <- list("accuracy")
resultList3 <- list("attrspace")
resultList4 <- list("accuracyOld")
par <- runif(1, 0.1, 1)
j<- par
flag<-0
accuracy<-0.6
accuracyOld<-0.2
max<--1
maxattrspace<--1
count<-0
while(accuracy<0.8 || count<=5)
{
  accuracyOld<-accuracy
  ebrmodel <- ebr(ds$train, "RF",m=10,attr.space=j,subsample =0.75)
  prediction <- predict(ebrmodel, ds$test)
  result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
  result$labels
  accuracy<-result$multilabel[1]
  thresres <- multilabel_evaluate(ds$test, prediction, "bipartition")
  thresres[3] 
  if(accuracy>=max){
    max<-accuracy
    maxattrspace<-j 
  }
  resultList <- list.append(resultList,accuracy)
  resultList3 <- list.append(resultList3,j)
  resultList4 <- list.append(resultList4,accuracyOld)
  
  if (flag==0)
  {
    if(accuracy>=0.6)
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
    if(j>=0.2)
      j=j-0.1;
  }
  count=count+1
  if(count==5){
    break;
  }
}
resultListTotal <- cbind(resultList4,resultList,resultList3)
resultListTotal
max
maxattrspace
end_time <- Sys.time()
end_time - start_time
mem_used()

#hyperparameter tuning bayesian ebr model attrspace

library("rBayesianOptimization")
start_time <- Sys.time()
dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
dataGroup <- dataGroup[,-1]
###this block gets specific columns of the features
fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
Test_Fun <- function(x) {
  fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")  
  ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
  ebrmodel <- ebr(ds$train, "RF", m=10, subsample=0.75,attr.space = x)
  predictions <- predict(ebrmodel, ds$test)
  results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
  results <- round(results, 4)
  list(Score = results[1],
       Pred = 0)
}
###Test_fun ending
## Set larger init_points and n_iter for better optimization result
OPT_Res <- BayesianOptimization(Test_Fun,
                                bounds = list(x = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)),
                                init_points = 2, n_iter = 10,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)
end_time <- Sys.time()
end_time - start_time
mem_used()

#nelder mead ebr model

start_time <- Sys.time()
optimize <-function(x){  
  dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")  
  dataGroup <- dataGroup[,-1]  
  ###this block gets specific columns of the features    
  fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
  ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))  
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")     
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    ebrmodel <- ebr(ds$train, "RF", m=10, attr.space = x)
    predictions <- predict(ebrmodel, ds$test)
    results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
    results <- round(results, 4)
    results[1]
}
library(nloptr)
# Bounded version of Nelder-Mead
lower <- 0.1
upper <- 1
S <- neldermead(0.2, optimize, lower, upper, nl.info = TRUE)
end_time <- Sys.time()
end_time - start_time
mem_used()

#EBR Grid Search attr.space

count <-1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")
start_time <- Sys.time()
while(count2 <= 5){
  sTime <- Sys.time()
  count <-1
  count2 <- count2+1  
  hyper <- runif(1, 0.1, 1)
  while(count<count2){
    hyperOpt <- sample(hyper,1,replace = FALSE)
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
    dataGroup <- dataGroup[,-1]
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    class(fileM)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    ebrmodel <- ebr(ds$train, "RF",m=10,attr.space=hyperOpt,subsample =0.75)   
    prediction <- predict(ebrmodel, ds$test)
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    accuracy<-result$multilabel[1]   
    count <- count+1
    if(count2==5){
      break;
    }
    
  }
  s2Time <- Sys.time()
  matris <- list.append(matris,count2)
  matris2 <- list.append(matris2,s2Time-sTime)
  matris3 <- list.append(matris3,accuracy)
  matris4 <- list.append(matris4,hyperOpt)
  if(count2==5){
    break;
  }
}
Total <- cbind(matris,matris2,matris3,matris4)

write.csv(Total,"C:/Desktop/result1.csv")
end_time <- Sys.time()
end_time - start_time
mem_used()

#EBR Random Search attr.space

count <-1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")
start_time <- Sys.time()
while(count2 <= 5){
  sTime <- Sys.time()
  count <-1
  count2 <- count2+1  
  hyper <- runif(1, 0.1, 1)
  while(count<count2){
    hyperOpt <- sample(hyper,1,replace = TRUE)
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
    dataGroup <- dataGroup[,-1]
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    class(fileM)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    ebrmodel <- ebr(ds$train, "RF",m=10,attr.space=hyperOpt,subsample =0.75)    
    prediction <- predict(ebrmodel, ds$test)   
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    accuracy<-result$multilabel[1]    
    count <- count+1
    if(count2==5){
      break;
    }    
  }
  s2Time <- Sys.time()
  matris <- list.append(matris,count2)
  matris2 <- list.append(matris2,s2Time-sTime)
  matris3 <- list.append(matris3,accuracy)
  matris4 <- list.append(matris4,hyperOpt)
  if(count2==5){
    break;
  }
}
Total <- cbind(matris,matris2,matris3,matris4)

write.csv(Total,"C:/Desktop/ebrrandom.csv")
end_time <- Sys.time()
end_time - start_time
mem_used()

#EBR Grid Search SUBSAMPLE

count <-1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")
start_time <- Sys.time()
while(count2 <= 5){
  sTime <- Sys.time()
  count <-1
  count2 <- count2+1  
  hyper <- runif(1, 0.1, 1)
  while(count<count2){
    hyperOpt <- sample(hyper,1,replace = FALSE)
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
    dataGroup <- dataGroup[,-1]
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    class(fileM)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    ebrmodel <- ebr(ds$train, "RF",m=10,subsample =hyperOpt)  
    prediction <- predict(ebrmodel, ds$test)   
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    accuracy<-result$multilabel[1]
    count <- count+1
    if(count2==5){
      break;
    }    
  }
  s2Time <- Sys.time()
  matris <- list.append(matris,count2)
  matris2 <- list.append(matris2,s2Time-sTime)
  matris3 <- list.append(matris3,accuracy)
  matris4 <- list.append(matris4,hyperOpt)
  if(count2==5){
    break;
  }
}
Total <- cbind(matris,matris2,matris3,matris4)
write.csv(Total,"C:/Desktop/ebrgridsample.csv")
end_time <- Sys.time()
end_time - start_time
mem_used()

#EBR Random Search SUBSAMPLE

count <-1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")
start_time <- Sys.time()
while(count2 <= 5){
  sTime <- Sys.time()
  count <-1
  count2 <- count2+1  
  hyper <- runif(1, 0.1, 1)
  while(count<count2){
    hyperOpt <- sample(hyper,1,replace = TRUE)
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
    dataGroup <- dataGroup[,-1]
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    class(fileM)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    ebrmodel <- ebr(ds$train, "RF",m=10,subsample =hyperOpt)
    prediction <- predict(ebrmodel, ds$test)  
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    accuracy<-result$multilabel[1]   
    count <- count+1
    if(count2==5){
      break;
    }
    
  }
  s2Time <- Sys.time()
  matris <- list.append(matris,count2)
  matris2 <- list.append(matris2,s2Time-sTime)
  matris3 <- list.append(matris3,accuracy)
  matris4 <- list.append(matris4,hyperOpt)
  if(count2==5){
    break;
  }
}
Total <- cbind(matris,matris2,matris3,matris4)
write.csv(Total,"C:/Desktop/ebrrandomsample.csv")
end_time <- Sys.time()
end_time - start_time
mem_used()

  
