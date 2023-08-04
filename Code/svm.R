# Import required libraries
library("e1071")
library(caret)
library(rlist)
library("base")
library("tm")
library(caTools)
library(pryr)
# Proposed method for SVM degree optimization
start_time <- Sys.time()
# Read the dataset
dataset <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
# Split data into training and testing sets
split <- sample.split(dataset, SplitRatio = 0.7)
degree <- c(1:10)
j <- degree[1]
flag <- 0
accuracy <- 0.6
accuracyOld <- 0.2
max <- -1
maxdegree <- -1
# Initialize lists to store results
resultList <- list("accuracy")
resultList3 <- list("degree")
resultList4 <- list("accuracyOld")
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
count <- 0
# Hyperparameter optimization loop
while (accuracy < 0.70 || count <= 25) { ### You can change values.
  # Train SVM model with current degree value
  svm_model <- svm(y22 ~ ., data = train, type = 'C-classification', kernel = 'polynomial', degree = j)  
  # Make predictions on the test set
  pred <- predict(svm_model, test)  
  # Compute confusion matrix
  cm <- table(test$y22, pred)  
  # Calculate accuracy
  accuracy <- sum(cm[1], cm[6], cm[11], cm[16]) / sum(cm[1:16])
   # Update maximum accuracy and corresponding degree value
  if (accuracy >= max) {
    max <- accuracy
    maxdegree <- j 
  }  
  # Store accuracy and degree values in lists
  resultList <- list.append(resultList, accuracy)
  resultList3 <- list.append(resultList3, j)
  resultList4 <- list.append(resultList4, accuracyOld)  
  # Control parameter updates based on accuracy
  if (flag == 0) {
    if (accuracy >= 0.65)
      j <- j + 1;
  }
  flag = 1;  
  if ((accuracy > accuracyOld) && (accuracy - accuracyOld) >= 0.001) {
    if (j <= 9)
      j <- j + 1;
  } else {
    if (j >= 2)
      j <- j - 1;
  }  
  count <- count + 1
    # Terminate the loop after 25 iterations
  if (count == 25) {
    break;
  }
}
# Combine the lists to create a final matrix with results
resultListTotal <- cbind(resultList4, resultList, resultList3)
resultListTotal
max
maxdegree
# Calculate the total execution time and memory usage
end_time <- Sys.time()
end_time - start_time
mem_used()

#svm hyperparameter tuning random and grid search for cost

# SVM hyperparameter tuning using Grid Search
start_time <- Sys.time()

# Read the dataset and split into training and testing sets
dataset <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")

# Convert the target variable to a factor
train$y22 <- as.factor(train$y22)

# Set up the train control for cross-validation
train_control = trainControl(method = "cv", number = 5, search = "grid")

# Define the parameter grid for hyperparameter tuning
tunegrid <- expand.grid(degree = 3, scale = 1, C = c(1:10))

# Train the SVM model using polynomial kernel and grid search for hyperparameter tuning
svm_model <- train(y22 ~ ., 
                   data = train, 
                   method = 'svmPoly', 
                   metric = 'Accuracy',
                   tuneGrid = tunegrid,
                   trControl = train_control
                  )
print(svm_model)
# Calculate execution time and memory usage
end_time <- Sys.time()
end_time - start_time
mem_used()

# SVM hyperparameter tuning using Bayesian Optimization
library(MlBayesOpt)
start_time <- Sys.time()

# Read the dataset and split into training and testing sets
dataset <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
train$y22 <- as.factor(train$y22)
test <- subset(dataset, split == "FALSE")
test$y22 <- as.factor(test$y22)

# Perform SVM hyperparameter tuning using Bayesian optimization
res0 <- svm_opt(train_data = train,
                train_label = y22,
                test_data = test,
                test_label = y22,
                svm_kernel = "sigmoid",
                init_points = 10,
                n_iter = 1)

# Calculate execution time and memory usage
end_time <- Sys.time()
end_time - start_time
mem_used()

#svm hyperparameter tuning random and grid search gamma

# SVM hyperparameter tuning using Random Search
start_time <- Sys.time()
# Read the dataset and split into training and testing sets
dataset <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
# Convert the target variable to a factor
train$y22 <- as.factor(train$y22)
# Set up the train control for cross-validation with random search
train_control = trainControl(method = "cv", number = 5, search = "random")
# Define the parameter grid for hyperparameter tuning with random search
tunegrid <- expand.grid(gamma = c(0.001, 0.1), scale = 1, C = 1)
# Train the SVM model using polynomial kernel and random search for hyperparameter tuning
svm_model <- train(y22 ~ ., 
                   data = train, 
                   method = 'svmPoly', 
                   metric = 'Accuracy',
                   tuneGrid = tunegrid,
                   trControl = train_control
                  )
print(svm_model)
# Calculate execution time and memory usage
end_time <- Sys.time()
end_time - start_time
mem_used()

#neldermead cost
# SVM hyperparameter tuning using Nelder-Mead optimization
start_time <- Sys.time()

# Define the optimize function for SVM with polynomial kernel
optimize <- function(cost) {
  dataset <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
  split <- sample.split(dataset, SplitRatio = 0.7) 
  train <- subset(dataset, split == "TRUE")
  test <- subset(dataset, split == "FALSE")
  svm_model <- svm(y22 ~ ., data = train, type = 'C-classification', kernel = 'polynomial', cost = cost)
  pred <- predict(svm_model, test)
  cm <- table(test$y22, pred) 
  accuracy <- sum(cm[1], cm[6], cm[11], cm[16]) / sum(cm[1:16])
  accuracy  
}
# Set the lower and upper bounds for cost hyperparameter
lower <- 1
upper <- 10
# Perform Nelder-Mead optimization
S <- neldermead(2, optimize, lower, upper, nl.info = TRUE)
# Calculate execution time and memory usage
end_time <- Sys.time()
end_time - start_time
mem_used()

#neldermead gamma
# SVM hyperparameter tuning using Nelder-Mead optimization
start_time <- Sys.time()
# Define the optimize function for SVM with polynomial kernel
optimize <- function(gamma) {
  dataset <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
  split <- sample.split(dataset, SplitRatio = 0.7) 
  train <- subset(dataset, split == "TRUE")
  test <- subset(dataset, split == "FALSE")
  svm_model <- svm(y22 ~ ., data = train, type = 'C-classification', kernel = 'polynomial', gamma = gamma)
  pred <- predict(svm_model, test)
  cm <- table(test$y22, pred) 
  accuracy <- sum(cm[1], cm[6], cm[11], cm[16]) / sum(cm[1:16])
  accuracy
}
# Set the lower and upper bounds for gamma hyperparameter
lower <- 0.001
upper <- 0.01
# Perform Nelder-Mead optimization
S <- neldermead(0.002, optimize, lower, upper, nl.info = TRUE)
# Calculate execution time and memory usage
end_time <- Sys.time()
end_time - start_time
mem_used()


#proposed method svm gamma

# SVM hyperparameter tuning for polynomial kernel using a custom method
start_time <- Sys.time()

# Read the dataset
dataset <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")

# Split the dataset into train and test sets
split <- sample.split(dataset, SplitRatio = 0.7)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")

# Generate a random value for gamma between 0.001 and 0.1
par <- runif(1, 0.001, 0.1)
j <- par
flag <- 0
accuracy <- 0.6
accuracyOld <- 0.2
max <- -1
maxgamma <- -1
resultList <- list("accuracy")
resultList2 <- list("ntree")
resultList3 <- list("gamma")
resultList4 <- list("accuracyOld")
count <- 0

# Hyperparameter tuning loop
while (accuracy < 0.55 || count <= 25) {
  # Train the SVM model with the current gamma value
  svm_model <- svm(y22 ~ ., data = train, type = 'C-classification', kernel = 'polynomial', gamma = j)
  pred <- predict(svm_model, test)
  cm <- table(test$y22, pred)
  accuracy <- sum(cm[1], cm[6], cm[11], cm[16]) / sum(cm[1:16])
  
  if (accuracy >= max) {
    max <- accuracy
    maxgamma <- j
  }
  
  resultList <- list.append(resultList, accuracy)
  resultList3 <- list.append(resultList3, j)
  resultList4 <- list.append(resultList4, accuracyOld)
  
  if (flag == 0) {
    if (accuracy >= 0.55)
      j <- j + 0.1
  }
  
  flag = 1
  if ((accuracy > accuracyOld) && (accuracy - accuracyOld) >= 0.001) {
    if (j <= 0.9)
      j <- j + 0.1
  } else {
    if (j >= 0.002)
      j <- j - 0.1
  }
  
  count <- count + 1
  if (count == 25) {
    break
  }
}

resultListTotal <- cbind(resultList4, resultList, resultList3)
resultListTotal
max
maxgamma
end_time <- Sys.time()
end_time - start_time
mem_used()

#proposed method svm cost

# SVM hyperparameter tuning for polynomial kernel with C-cost parameter
start_time <- Sys.time()

# Read the dataset
dataset <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")

# Split the dataset into train and test sets
split <- sample.split(dataset, SplitRatio = 0.7)

# Generate a random value for C-cost parameter between 0.001 and 0.1
par <- runif(1, 0.001, 0.1)
j <- par
flag <- 0
accuracy <- 0.6
accuracyOld <- 0.2
max <- -1
maxcost <- -1
resultList <- list("accuracy")
resultList3 <- list("cost")
resultList4 <- list("accuracyOld")
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
count <- 0

# Hyperparameter tuning loop
while (accuracy < 0.80 || count <= 25) {
  # Train the SVM model with the current C-cost parameter value
  svm_model <- svm(y22 ~ ., data = train, type = 'C-classification', kernel = 'polynomial', cost = j)
  pred <- predict(svm_model, test)
  cm <- table(test$y22, pred)
  accuracy <- sum(cm[1], cm[6], cm[11], cm[16]) / sum(cm[1:16])
  
  if (accuracy >= max) {
    max <- accuracy
    maxcost <- j
  }
  
  resultList <- list.append(resultList, accuracy)
  resultList3 <- list.append(resultList3, j)
  resultList4 <- list.append(resultList4, accuracyOld)
  
  if (flag == 0) {
    if (accuracy >= 0.55)
      j <- j + 1
  }
  
  flag = 1
  if ((accuracy > accuracyOld) && (accuracy - accuracyOld) >= 0.001) {
    if (j <= 0.9)
      j <- j + 0.1
  } else {
    if (j >= 0.002)
      j <- j - 0.1
  }
  
  count <- count + 1
  if (count == 25) {
    break
  }
}

resultListTotal <- cbind(resultList4, resultList, resultList3)
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
while(count2 <= 27){ ### The loop continues as long as count2 is less than or equal to 27. Initially, count2 is set to 3, so the loop will run at least once.
  sTime <- Sys.time()
  count <-1
  count2 <- count2+1  
  hyper <- runif(1, 0.001, 0.1) ### In the inner loop, a random gamma value (hyper) between 0.001 and 0.1 is generated. This means a different gamma value will be selected for each iteration.
  while(count<count2){
    hyperOpt <- sample(hyper,1,replace = FALSE)
    svm_model <- svm(y22 ~ ., data=train, type = 'C-classification',kernel = 'polynomial', gamma=hyperOpt) ### The trained model is then used to make predictions on the test set, and the accuracy value is calculated. 
    ###Accuracy represents the ratio of correctly classified examples to the total number of examples.
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
  hyper <- runif(1, 0.001, 0.1) ### Inside the inner loop, a random hyperOpt value is sampled from the range (0.001, 0.1) with replacement. This value will be used as the gamma hyperparameter for the SVM model.
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
