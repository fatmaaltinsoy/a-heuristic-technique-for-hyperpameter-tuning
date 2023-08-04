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
fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") ### Convert the dataset into an mldr object (multi-label data representation) named fileM. The labels (y1 to y23) are specified as the label indices (c(51:72)).
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
while(accuracy<0.8 || count<=5) ###In each iteration, a binary relevance ensemble (EBR) model is trained using the Random Forest (RF) algorithm as the base learner ("RF"). 
  ###The number of base learners is set to 10 (m=10), and the attr.space parameter is set to the current value of j. You can change values.
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

# Load necessary libraries, including rBayesianOptimization.
library("rBayesianOptimization")

# Start the timer to measure the code execution time.
start_time <- Sys.time()

# Read the data from the CSV file "C:/Users/Desktop/stack/processedData.csv" into the 'dataGroup' variable.
dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")

# Remove the first column from the dataset as it appears to be an index or identifier column.
dataGroup <- dataGroup[,-1]

# Convert the dataset into an 'mldr' object named 'fileM', specifying the labels (y1 to y23) as label indices (c(51:72)).
fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")

# Create a holdout partition from the 'fileM' dataset with a training set of 80% and a test set of 20%.
ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))

# Define the 'Test_Fun' function, which takes a single parameter 'x' as input.
# This function performs EBR with Random Forest as the base learner and evaluates the results using example-based accuracy and macro-F1 metrics.
Test_Fun <- function(x) {
  # Re-create the 'fileM' and 'ds' objects to avoid any conflicts with other parts of the code.
  fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")
  ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
  
  # Train an EBR model using Random Forest as the base learner, with the 'attr.space' parameter set to the value 'x'.
  ebrmodel <- ebr(ds$train, "RF", m=10, subsample=0.75, attr.space = x)
  
  # Make predictions on the test set using the trained EBR model.
  predictions <- predict(ebrmodel, ds$test)
  
  # Evaluate the predictions using the 'example-based' and 'macro-F1' metrics and round the results to 4 decimal places.
  results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
  results <- round(results, 4)
  
  # Return the evaluation score (example-based accuracy) and a constant value of 0 as the second element of the list.
  # The constant value is not used but seems to be required for the Bayesian optimization function.
  list(Score = results[1], Pred = 0)
}

# Define the optimization problem using the 'BayesianOptimization' function.
# The optimization is performed over the 'Test_Fun' function.
# The bounds for the 'attr.space' parameter are set to [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8].
# 'init_points' is set to 2, representing the number of random initial points for exploration.
# 'n_iter' is set to 10, representing the number of optimization steps.
# The acquisition function 'ucb' (Upper Confidence Bound) is used with a 'kappa' value of 2.576, and 'eps' is set to 0.0.
OPT_Res <- BayesianOptimization(Test_Fun,
                                bounds = list(x = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)),
                                init_points = 2, n_iter = 10,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

# Stop the timer and calculate the total execution time and memory usage of the code.
end_time <- Sys.time()
execution_time <- end_time - start_time
memory_usage <- mem_used()

# Print the execution time and memory usage.
print(execution_time)
print(memory_usage)


#nelder mead ebr model

# Load necessary libraries, including rBayesianOptimization.
library("rBayesianOptimization")

# Start the timer to measure the code execution time.
start_time <- Sys.time()

# Read the data from the CSV file "C:/Users/Desktop/stack/processedData.csv" into the 'dataGroup' variable.
dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")

# Remove the first column from the dataset as it appears to be an index or identifier column.
dataGroup <- dataGroup[,-1]

# Convert the dataset into an 'mldr' object named 'fileM', specifying the labels (y1 to y23) as label indices (c(51:72)).
fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")

# Create a holdout partition from the 'fileM' dataset with a training set of 80% and a test set of 20%.
ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))

# Define the 'Test_Fun' function, which takes a single parameter 'x' as input.
# This function performs EBR with Random Forest as the base learner and evaluates the results using example-based accuracy and macro-F1 metrics.
Test_Fun <- function(x) {
  # Re-create the 'fileM' and 'ds' objects to avoid any conflicts with other parts of the code.
  fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")
  ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
  
  # Train an EBR model using Random Forest as the base learner, with the 'attr.space' parameter set to the value 'x'.
  ebrmodel <- ebr(ds$train, "RF", m=10, subsample=0.75, attr.space = x)
  
  # Make predictions on the test set using the trained EBR model.
  predictions <- predict(ebrmodel, ds$test)
  
  # Evaluate the predictions using the 'example-based' and 'macro-F1' metrics and round the results to 4 decimal places.
  results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
  results <- round(results, 4)
  
  # Return the evaluation score (example-based accuracy) and a constant value of 0 as the second element of the list.
  # The constant value is not used but seems to be required for the Bayesian optimization function.
  list(Score = results[1], Pred = 0)
}

# Define the optimization problem using the 'BayesianOptimization' function.
# The optimization is performed over the 'Test_Fun' function.
# The bounds for the 'attr.space' parameter are set to [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8].
# 'init_points' is set to 2, representing the number of random initial points for exploration.
# 'n_iter' is set to 10, representing the number of optimization steps.
# The acquisition function 'ucb' (Upper Confidence Bound) is used with a 'kappa' value of 2.576, and 'eps' is set to 0.0.
OPT_Res <- BayesianOptimization(Test_Fun,
                                bounds = list(x = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)),
                                init_points = 2, n_iter = 10,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

# Stop the timer and calculate the total execution time and memory usage of the code.
end_time <- Sys.time()
execution_time <- end_time - start_time
memory_usage <- mem_used()

# Print the execution time and memory usage.
print(execution_time)
print(memory_usage)


#EBR Grid Search attr.space

# Initialize counters and result matrices/lists.
count <- 1
count2 <- 2
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Start the timer to measure the code execution time.
start_time <- Sys.time()

# The outer loop runs until count2 reaches 5.
while (count2 <= 5) {
  # Record the starting time of the inner loop.
  sTime <- Sys.time()
  
  # Reset the inner loop counter.
  count <- 1
  count2 <- count2 + 1
  
  # Generate a random value for the hyperparameter 'attr.space' in the range [0.1, 1].
  hyper <- runif(1, 0.1, 1)
  
  # The inner loop runs until count reaches count2.
  while (count < count2) {
    # Sample a random value of 'attr.space' from the previously generated 'hyper' value.
    hyperOpt <- sample(hyper, 1, replace = FALSE)
    
    # Read the data from the CSV file "C:/Users/Desktop/stack/processedData.csv" into the 'dataGroup' variable.
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
    
    # Remove the first column from the dataset as it appears to be an index or identifier column.
    dataGroup <- dataGroup[, -1]
    
    # Convert the dataset into an 'mldr' object named 'fileM', specifying the labels (y1 to y23) as label indices (c(51:72)).
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")
    
    # Create a holdout partition from the 'fileM' dataset with a training set of 80% and a test set of 20%.
    ds <- create_holdout_partition(fileM, c(train = 0.8, test = 0.2))
    
    # Train an EBR model using Random Forest as the base learner, with the 'attr.space' parameter set to the value 'hyperOpt'.
    ebrmodel <- ebr(ds$train, "RF", m = 10, attr.space = hyperOpt, subsample = 0.75)
    
    # Make predictions on the test set using the trained EBR model.
    prediction <- predict(ebrmodel, ds$test)
    
    # Evaluate the predictions using the 'bipartition' metric and get the accuracy.
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels = TRUE)
    accuracy <- result$multilabel[1]
    
    # Increment the inner loop counter.
    count <- count + 1
    
    # If count2 reaches 5, break the inner loop.
    if (count2 == 5) {
      break;
    }
  }
  
  # Record the ending time of the inner loop.
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and hyperparameter value to the corresponding lists.
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # If count2 reaches 5, break the outer loop.
  if (count2 == 5) {
    break;
  }
}

# Combine the lists/matrices into a single data frame.
Total <- cbind(matris, matris2, matris3, matris4)

# Write the results to a CSV file named "result1.csv" in the "C:/Desktop/" directory.
write.csv(Total, "C:/Desktop/result1.csv")

# Stop the timer and calculate the total execution time and memory usage of the code.
end_time <- Sys.time()
execution_time <- end_time - start_time
memory_usage <- mem_used()

# Print the execution time and memory usage.
print(execution_time)
print(memory_usage)

#EBR Random Search attr.space

# Initialize counters and result matrices/lists.
count <- 1
count2 <- 2
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Start the timer to measure the code execution time.
start_time <- Sys.time()

# The outer loop runs until count2 reaches 5.
while (count2 <= 5) {
  # Record the starting time of the inner loop.
  sTime <- Sys.time()
  
  # Reset the inner loop counter.
  count <- 1
  count2 <- count2 + 1
  
  # Generate a random value for the hyperparameter 'hyperOpt' in the range [0.1, 1].
  hyper <- runif(1, 0.1, 1)
  
  # The inner loop runs until count reaches count2.
  while (count < count2) {
    # Sample a random value of 'hyperOpt' from the previously generated 'hyper' value.
    hyperOpt <- sample(hyper, 1, replace = TRUE)
    
    # Read the data from the CSV file "C:/Users/Desktop/stack/processedData.csv" into the 'dataGroup' variable.
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
    
    # Remove the first column from the dataset as it appears to be an index or identifier column.
    dataGroup <- dataGroup[, -1]
    
    # Convert the dataset into an 'mldr' object named 'fileM', specifying the labels (y1 to y23) as label indices (c(51:72)).
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")
    
    # Create a holdout partition from the 'fileM' dataset with a training set of 80% and a test set of 20%.
    ds <- create_holdout_partition(fileM, c(train = 0.8, test = 0.2))
    
    # Train an EBR model using Random Forest as the base learner, with the 'hyperOpt' parameter.
    ebrmodel <- ebr(ds$train, "RF", m = 10, attr.space = hyperOpt, subsample = 0.75)
    
    # Make predictions on the test set using the trained EBR model.
    prediction <- predict(ebrmodel, ds$test)
    
    # Evaluate the predictions using the 'bipartition' metric and get the accuracy.
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels = TRUE)
    accuracy <- result$multilabel[1]
    
    # Increment the inner loop counter.
    count <- count + 1
    
    # If count2 reaches 5, break the inner loop.
    if (count2 == 5) {
      break;
    }
  }
  
  # Record the ending time of the inner loop.
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and hyperparameter value to the corresponding lists.
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # If count2 reaches 5, break the outer loop.
  if (count2 == 5) {
    break;
  }
}

# Combine the lists/matrices into a single data frame.
Total <- cbind(matris, matris2, matris3, matris4)

# Write the results to a CSV file named "ebrrandom.csv" in the "C:/Desktop/" directory.
write.csv(Total, "C:/Desktop/ebrrandom.csv")

# Stop the timer and calculate the total execution time and memory usage of the code.
end_time <- Sys.time()
execution_time <- end_time - start_time
memory_usage <- mem_used()

# Print the execution time and memory usage.
print(execution_time)
print(memory_usage)


#EBR Grid Search SUBSAMPLE

# Initialize counters and result matrices/lists.
count <- 1
count2 <- 2
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Start the timer to measure the code execution time.
start_time <- Sys.time()

# The outer loop runs until count2 reaches 5.
while (count2 <= 5) {
  # Record the starting time of the inner loop.
  sTime <- Sys.time()
  
  # Reset the inner loop counter.
  count <- 1
  count2 <- count2 + 1
  
  # Generate a random value for the hyperparameter 'hyperOpt' in the range [0.1, 1].
  hyper <- runif(1, 0.1, 1)
  
  # The inner loop runs until count reaches count2.
  while (count < count2) {
    # Sample a random value of 'hyperOpt' from the previously generated 'hyper' value.
    hyperOpt <- sample(hyper, 1, replace = FALSE)
    
    # Read the data from the CSV file "C:/Users/Desktop/stack/processedData.csv" into the 'dataGroup' variable.
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
    
    # Remove the first column from the dataset as it appears to be an index or identifier column.
    dataGroup <- dataGroup[, -1]
    
    # Convert the dataset into an 'mldr' object named 'fileM', specifying the labels (y1 to y23) as label indices (c(51:72)).
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")
    
    # Create a holdout partition from the 'fileM' dataset with a training set of 80% and a test set of 20%.
    ds <- create_holdout_partition(fileM, c(train = 0.8, test = 0.2))
    
    # Train an EBR model using Random Forest as the base learner, with the 'hyperOpt' parameter.
    ebrmodel <- ebr(ds$train, "RF", m = 10, subsample = hyperOpt)
    
    # Make predictions on the test set using the trained EBR model.
    prediction <- predict(ebrmodel, ds$test)
    
    # Evaluate the predictions using the 'bipartition' metric and get the accuracy.
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels = TRUE)
    accuracy <- result$multilabel[1]
    
    # Increment the inner loop counter.
    count <- count + 1
    
    # If count2 reaches 5, break the inner loop.
    if (count2 == 5) {
      break;
    }
  }
  
  # Record the ending time of the inner loop.
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and hyperparameter value to the corresponding lists.
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # If count2 reaches 5, break the outer loop.
  if (count2 == 5) {
    break;
  }
}

# Combine the lists/matrices into a single data frame.
Total <- cbind(matris, matris2, matris3, matris4)

# Write the results to a CSV file named "ebrgridsample.csv" in the "C:/Desktop/" directory.
write.csv(Total, "C:/Desktop/ebrgridsample.csv")

# Stop the timer and calculate the total execution time and memory usage of the code.
end_time <- Sys.time()
execution_time <- end_time - start_time
memory_usage <- mem_used()

# Print the execution time and memory usage.
print(execution_time)
print(memory_usage)


#EBR Random Search SUBSAMPLE

# Initialize counters and result matrices/lists.
count <- 1
count2 <- 2
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Start the timer to measure the code execution time.
start_time <- Sys.time()

# The outer loop runs until count2 reaches 5.
while (count2 <= 5) {
  # Record the starting time of the inner loop.
  sTime <- Sys.time()
  
  # Reset the inner loop counter.
  count <- 1
  count2 <- count2 + 1
  
  # Generate a random value for the hyperparameter 'hyperOpt' in the range [0.1, 1].
  hyper <- runif(1, 0.1, 1)
  
  # The inner loop runs until count reaches count2.
  while (count < count2) {
    # Sample a random value of 'hyperOpt' from the previously generated 'hyper' value.
    hyperOpt <- sample(hyper, 1, replace = TRUE)
    
    # Read the data from the CSV file "C:/Users/Desktop/stack/processedData.csv" into the 'dataGroup' variable.
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedData.csv")
    
    # Remove the first column from the dataset as it appears to be an index or identifier column.
    dataGroup <- dataGroup[, -1]
    
    # Convert the dataset into an 'mldr' object named 'fileM', specifying the labels (y1 to y23) as label indices (c(51:72)).
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")
    
    # Create a holdout partition from the 'fileM' dataset with a training set of 80% and a test set of 20%.
    ds <- create_holdout_partition(fileM, c(train = 0.8, test = 0.2))
    
    # Train an EBR model using Random Forest as the base learner, with the 'hyperOpt' parameter.
    ebrmodel <- ebr(ds$train, "RF", m = 10, subsample = hyperOpt)
    
    # Make predictions on the test set using the trained EBR model.
    prediction <- predict(ebrmodel, ds$test)
    
    # Evaluate the predictions using the 'bipartition' metric and get the accuracy.
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels = TRUE)
    accuracy <- result$multilabel[1]
    
    # Increment the inner loop counter.
    count <- count + 1
    
    # If count2 reaches 5, break the inner loop.
    if (count2 == 5) {
      break;
    }
  }
  
  # Record the ending time of the inner loop.
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and hyperparameter value to the corresponding lists.
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # If count2 reaches 5, break the outer loop.
  if (count2 == 5) {
    break;
  }
}

# Combine the lists/matrices into a single data frame.
Total <- cbind(matris, matris2, matris3, matris4)

# Write the results to a CSV file named "ebrrandomsample.csv" in the "C:/Desktop/" directory.
write.csv(Total, "C:/Desktop/ebrrandomsample.csv")

# Stop the timer and calculate the total execution time and memory usage of the code.
end_time <- Sys.time()
execution_time <- end_time - start_time
memory_usage <- mem_used()

# Print the execution time and memory usage.
print(execution_time)
print(memory_usage)


  
