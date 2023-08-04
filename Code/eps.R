# Load the required libraries
library("utiml")
library("mldr")
library("parallel")
library("ROCR")

# Record the start time of the script execution
start_time <- Sys.time()

# Generate a random seed for reproducibility and select 100 random instances from the dataset
set.seed(123)
randomN <- sample(1:1000, 100)

# Read the dataset from the CSV file "C:/Users/Desktop/stack/processedDataset.csv"
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")

# Remove the first column of the dataset (assuming it contains row indices)
dataGroup <- dataGroup[,-1]

# Convert the dataset into an 'mldr' object named 'fileM'
# Specify columns 51 to 72 as the label indices (y1 to y23)
fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 

# Split the 'fileM' dataset into training and test sets (80% training, 20% test)
ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))

# Initialize lists to store the results
resultList <- list("accuracy")
resultList3 <- list("attrspace")
resultList4 <- list("accuracyOld")

# Initialize the 'subsample' hyperparameter 'j' with a random value between 0.1 and 1
par <- runif(1, 0.1, 1)
j <- par

# Initialize other variables for tracking the experiment
flag <- 0
accuracy <- 0.6
accuracyOld <- 0.2
max <- -1
maxsubsample <- -1
count <- 0

# Perform the experiment until the accuracy reaches 0.70 or count reaches 5
while(accuracy < 0.70 || count <= 5) {
  # Store the current accuracy in 'accuracyOld'
  accuracyOld <- accuracy
  
  # Train the EPS model with the 'RF' base learner and the current 'subsample' value 'j'
  model <- eps(ds$train, "RF", m=10, subsample=j, p=3, strategy="B") 
  
  # Make predictions on the test set using the trained model
  prediction <- predict(model, ds$test)
  
  # Evaluate the model's performance using the "bipartition" metric and obtain the labels
  result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
  result$labels
  
  # Evaluate the model's performance using the "bipartition" metric
  thresres <- multilabel_evaluate(ds$test, prediction, "bipartition")
  thresres[3]
  
  # Store the accuracy value in the variable 'accuracy'
  accuracy <- result$multilabel[1]
  accuracy 
  
  # Update the maximum accuracy and the corresponding 'subsample' value
  if(accuracy >= max) {
    max <- accuracy
    maxsubsample <- j 
  }
  
  # Append the current accuracy and 'subsample' value to the respective lists
  resultList <- list.append(resultList, accuracy)
  resultList3 <- list.append(resultList3, j)
  resultList4 <- list.append(resultList4, accuracyOld)
  
  # If 'flag' is 0 (first iteration), check if the accuracy is at least 0.6 to increase 'subsample' value 'j'
  if (flag == 0) {
    if(accuracy >= 0.6)
      j <- j + 1;
  }
  
  # Set 'flag' to 1 after the first iteration
  flag <- 1;
  
  # Adjust the 'subsample' value 'j' based on the change in accuracy
  if((accuracy > accuracyOld) && (accuracy - accuracyOld) >= 0.001) {
    if(j <= 3)
      j <- j + 1;
  } else {
    if(j >= 2)
      j <- j - 1;
  }
  
  # Increment the count
  count <- count + 1
  
  # Break the loop if count reaches 5 to stop the experiment after 5 iterations
  if(count == 5){
    break;
  }
}

# Combine the lists 'resultList4', 'resultList', 'resultList3' into a single data frame 'resultListTotal'
resultListTotal <- cbind(resultList4, resultList, resultList3)

# Print the 'resultListTotal', maximum accuracy, and the corresponding 'subsample' value
resultListTotal
max
maxsubsample

# Record the end time of the script execution
end_time <- Sys.time()

# Calculate the elapsed time for the experiment
end_time - start_time

# Check the memory usage
mem_used()


#hyperparameter tuning bayesian eps model 

# Load necessary libraries
library("utiml")
library("mldr")
library("parallel")
library("ROCR")

# Record the start time of the script execution
start_time <- Sys.time()

# Read the dataset from the CSV file "C:/Users/Desktop/stack/processedDataset.csv"
dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
dataGroup <- dataGroup[,-1]

# Convert the dataset into an 'mldr' object named 'fileM'
# Specify columns 51 to 72 as the label indices (y1 to y23)
fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 

# Split the 'fileM' dataset into training and test sets (80% training, 20% test)
ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))

# Define the objective function for Bayesian optimization
# The function takes 'x' as the 'b' hyperparameter of the EPS model
Test_Fun <- function(x) {
  fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")   
  ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
  
  # Train an EPS model with 'RF' base learner and given 'b' hyperparameter
  epsmodel <- eps(ds$train, "RF", m=15, subsample = 0.63, p=3, strategy="B", b=x)
  
  # Make predictions on the test set using the trained model
  predictions <- predict(epsmodel, ds$test)
  
  # Evaluate the model's performance using "example-based" and "macro-F1" metrics
  results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
  
  # Round the evaluation results to four decimal places
  results <- round(results, 4)
  
  # Return the optimization score (first element of 'results') and 'Pred' value as 0
  list(Score = results[1], Pred = 0)
}

# Bayesian Optimization: Find the optimal value of 'b' using the defined objective function
OPT_Res <- BayesianOptimization(Test_Fun,
                                bounds = list(x = c(1, 2, 3, 4)),
                                init_points = 2, n_iter = 10,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

# Record the end time of the script execution
end_time <- Sys.time()

# Calculate the elapsed time for the optimization process
end_time - start_time

# Check the memory usage
mem_used()


#nelder mead eps model

# Record the start time of the script execution
start_time <- Sys.time()

# Define the optimization function 'optimize'
# It takes 'x' as the 'b' hyperparameter for the EPS model
optimize <- function(x) {  
  # Read the dataset from the CSV file "C:/Users/Desktop/stack/processedDataset.csv"
  dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")  
  
  # Remove the first column of the dataset (assuming it contains row indices)
  dataGroup <- dataGroup[,-1]  
  
  # Convert the dataset into an 'mldr' object named 'fileM'
  # Specify columns 51 to 72 as the label indices (y1 to y23)
  fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM")   
  
  # Split the 'fileM' dataset into training and test sets (80% training, 20% test)
  ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
  
  # Train the EPS model with the 'RF' base learner and given 'b' hyperparameter
  epsmodel <- eps(ds$train, "RF", subsample = 0.63, p=3, strategy="B", b=x)
  
  # Make predictions on the test set using the trained model
  predictions <- predict(epsmodel, ds$test)
  
  # Evaluate the model's performance using "example-based" and "macro-F1" metrics
  results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
  
  # Round the evaluation results to four decimal places
  results <- round(results, 4)
  
  # Return the evaluation score corresponding to "example-based" metric
  results[1]
}

# Load the 'nloptr' library for optimization
library(nloptr)

# Define the lower and upper bounds for the 'b' hyperparameter
lower <- 1
upper <- 4

# Perform Nelder-Mead optimization with the 'optimize' function
# Starting point: 1 (You can change this)
# The optimization is bounded within the range [1, 4]
S <- neldermead(1, optimize, lower, upper, nl.info = TRUE)

# Record the end time of the script execution
end_time <- Sys.time()

# Calculate the elapsed time for the optimization process
end_time - start_time

# Check the memory usage
mem_used()


#EPS Random Search SUBSAMPLE

# Initialize variables
count <- 1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Record the start time of the script execution
start_time <- Sys.time()

# The outer while loop with count2 controls the number of iterations
# The experiment will be performed 5 times (count2 <= 5)
while(count2 <= 5){
  # Record the start time for each iteration
  sTime <- Sys.time()
  
  # Reset the count and increment count2
  count <- 1
  count2 <- count2 + 1  
  
  # Generate a random hyperparameter value 'hyperOpt' between 0.1 and 1
  hyper <- runif(1, 0.1, 1)
  
  # The inner while loop with count controls the number of random hyperparameter samples
  # In each iteration, it samples a random hyperparameter value from the range
  # And evaluates the EPS model's performance with that value
  while(count < count2){
    # Sample a random hyperparameter value 'hyperOpt' from the range
    hyperOpt <- sample(hyper, 1, replace = TRUE)
    
    # Load the dataset from the CSV file "C:/Users/Desktop/stack/processedDataset.csv"
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
    
    # Remove the first column of the dataset (assuming it contains row indices)
    dataGroup <- dataGroup[,-1]
    
    # Convert the dataset into an 'mldr' object named 'fileM'
    # Specify columns 51 to 72 as the label indices (y1 to y23)
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    
    # Split the 'fileM' dataset into training and test sets (80% training, 20% test)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    
    # Train the EPS model with the 'RF' base learner and the sampled 'hyperOpt' value
    model <- eps(ds$train, "RF", m=10, subsample = hyperOpt, p=3, strategy="B")
    
    # Make predictions on the test set using the trained model
    prediction <- predict(model, ds$test)
    
    # Evaluate the model's performance using the "bipartition" metric and obtain the labels
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    
    # Evaluate the model's performance using the "bipartition" metric
    thresres <- multilabel_evaluate(ds$test, prediction, "bipartition")
    thresres[3]
    
    # Store the accuracy value in the variable 'accuracy'
    accuracy <- result$multilabel[1]
    accuracy
    
    # Increment the count
    count <- count + 1
    
    # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
    if(count2 == 5){
      break;
    }  
  }
  
  # Record the end time for the current iteration
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and hyperparameter value to the respective lists
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
  if(count2 == 5){
    break;
  }
}

# Combine the lists 'matris', 'matris2', 'matris3', 'matris4' into a single data frame 'Total'
Total <- cbind(matris, matris2, matris3, matris4)

# Write the 'Total' data frame to a CSV file "C:/Desktop/epsrandomsample.csv"
write.csv(Total, "C:/Desktop/epsrandomsample.csv")

# Record the end time of the script execution
end_time <- Sys.time()

# Calculate the elapsed time for the experiment
end_time - start_time

# Check the memory usage
mem_used()


#EPS Grid Search SUBSAMPLE

# Initialize variables
count <- 1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Record the start time of the script execution
start_time <- Sys.time()

# The outer while loop with count2 controls the number of iterations
# The experiment will be performed 5 times (count2 <= 5)
while(count2 <= 5){
  # Record the start time for each iteration
  sTime <- Sys.time()
  
  # Reset the count and increment count2
  count <- 1
  count2 <- count2 + 1
  
  # Generate a random hyperparameter value 'hyper' between 0.1 and 1
  hyper <- runif(1, 0.1, 1)
  
  # The inner while loop with count controls the number of grid-based hyperparameter samples
  # In each iteration, it samples a random hyperparameter value from the range
  # And evaluates the EPS model's performance with that value
  while(count < count2){
    # Sample a random hyperparameter value 'hyperOpt' from the range without replacement
    hyperOpt <- sample(hyper, 1, replace = FALSE)
    
    # Load the dataset from the CSV file "C:/Users/Desktop/stack/processedDataset.csv"
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
    
    # Remove the first column of the dataset (assuming it contains row indices)
    dataGroup <- dataGroup[,-1]
    
    # Convert the dataset into an 'mldr' object named 'fileM'
    # Specify columns 51 to 72 as the label indices (y1 to y23)
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    
    # Split the 'fileM' dataset into training and test sets (80% training, 20% test)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    
    # Train the EPS model with the 'RF' base learner and the sampled 'hyperOpt' value
    model <- eps(ds$train, "RF", m=10, subsample = hyperOpt, p=3, strategy="B") 
    
    # Make predictions on the test set using the trained model
    prediction <- predict(model, ds$test)
    
    # Evaluate the model's performance using the "bipartition" metric and obtain the labels
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    
    # Evaluate the model's performance using the "bipartition" metric
    thresres <- multilabel_evaluate(ds$test, prediction, "bipartition")
    thresres[3]
    
    # Store the accuracy value in the variable 'accuracy'
    accuracy <- result$multilabel[1]
    accuracy
    
    # Increment the count
    count <- count + 1
    
    # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
    if(count2 == 5){
      break;
    }  
  }
  
  # Record the end time for the current iteration
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and hyperparameter value to the respective lists
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
  if(count2 == 5){
    break;
  }
}

# Combine the lists 'matris', 'matris2', 'matris3', 'matris4' into a single data frame 'Total'
Total <- cbind(matris, matris2, matris3, matris4)

# Write the 'Total' data frame to a CSV file "C:/Desktop/epsgridsample.csv"
write.csv(Total, "C:/Desktop/epsgridsample.csv")

# Record the end time of the script execution
end_time <- Sys.time()

# Calculate the elapsed time for the experiment
end_time - start_time

# Check the memory usage
mem_used()


#EPS Random Search p

# Initialize variables
count <- 1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Record the start time of the script execution
start_time <- Sys.time()

# The outer while loop with count2 controls the number of iterations
# The experiment will be performed 5 times (count2 <= 5)
while(count2 <= 5){
  # Record the start time for each iteration
  sTime <- Sys.time()
  
  # Reset the count and increment count2
  count <- 1
  count2 <- count2 + 1
  
  # Define the hyperparameter range from which 'hyperOpt' will be sampled
  hyper <- c(1:3)
  
  # The inner while loop with count controls the number of random-based hyperparameter samples
  # In each iteration, it samples a random hyperparameter value from the range
  # And evaluates the EPS model's performance with that value
  while(count < count2){
    # Sample a random hyperparameter value 'hyperOpt' from the range with replacement
    hyperOpt <- sample(hyper, 1, replace = TRUE)
    
    # Load the dataset from the CSV file "C:/Users/Desktop/stack/processedDataset.csv"
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
    
    # Remove the first column of the dataset (assuming it contains row indices)
    dataGroup <- dataGroup[,-1]
    
    # Convert the dataset into an 'mldr' object named 'fileM'
    # Specify columns 51 to 72 as the label indices (y1 to y23)
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    
    # Split the 'fileM' dataset into training and test sets (80% training, 20% test)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    
    # Train the EPS model with the 'RF' base learner and the sampled 'hyperOpt' value
    model <- eps(ds$train, "RF", m=10, p=hyperOpt, strategy="B")
    
    # Make predictions on the test set using the trained model
    prediction <- predict(model, ds$test)
    
    # Evaluate the model's performance using the "bipartition" metric and obtain the labels
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    
    # Evaluate the model's performance using the "bipartition" metric
    thresres <- multilabel_evaluate(ds$test, prediction, "bipartition")
    thresres[3]
    
    # Store the accuracy value in the variable 'accuracy'
    accuracy <- result$multilabel[1]
    accuracy
    
    # Increment the count
    count <- count + 1
    
    # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
    if(count2 == 5){
      break;
    }  
  }
  
  # Record the end time for the current iteration
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and the sampled hyperparameter value to the respective lists
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
  if(count2 == 5){
    break;
  }
}

# Combine the lists 'matris', 'matris2', 'matris3', 'matris4' into a single data frame 'Total'
Total <- cbind(matris, matris2, matris3, matris4)

# Write the 'Total' data frame to a CSV file "C:/Desktop/epsrandomp.csv"
write.csv(Total, "C:/Desktop/epsrandomp.csv")

# Record the end time of the script execution
end_time <- Sys.time()

# Calculate the elapsed time for the experiment
end_time - start_time

# Check the memory usage
mem_used()

#EPS grid Search p

# Initialize variables
count <- 1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Record the start time of the script execution
start_time <- Sys.time()

# The outer while loop with count2 controls the number of iterations
# The experiment will be performed 5 times (count2 <= 5)
while(count2 <= 5){
  # Record the start time for each iteration
  sTime <- Sys.time()
  
  # Reset the count and increment count2
  count <- 1
  count2 <- count2 + 1
  
  # Define the hyperparameter range 'hyper' as a sequence from 1 to 3
  hyper <- c(1:3)
  
  # The inner while loop with count controls the number of grid-based hyperparameter samples
  # In each iteration, it takes the next value from the 'hyper' range
  # And evaluates the EPS model's performance with that value
  while(count < count2){
    # Take the next value from the 'hyper' range as 'hyperOpt'
    hyperOpt <- sample(hyper, 1, replace = FALSE)
    
    # Load the dataset from the CSV file "C:/Users/Desktop/stack/processedDataset.csv"
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
    
    # Remove the first column of the dataset (assuming it contains row indices)
    dataGroup <- dataGroup[,-1]
    
    # Convert the dataset into an 'mldr' object named 'fileM'
    # Specify columns 51 to 72 as the label indices (y1 to y23)
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    
    # Split the 'fileM' dataset into training and test sets (80% training, 20% test)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    
    # Train the EPS model with the 'RF' base learner and the selected 'hyperOpt' value
    model <- eps(ds$train, "RF", m=10, p=hyperOpt, strategy="B")
    
    # Make predictions on the test set using the trained model
    prediction <- predict(model, ds$test)
    
    # Evaluate the model's performance using the "bipartition" metric and obtain the labels
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    
    # Evaluate the model's performance using the "bipartition" metric
    thresres <- multilabel_evaluate(ds$test, prediction, "bipartition")
    thresres[3]
    
    # Store the accuracy value in the variable 'accuracy'
    accuracy <- result$multilabel[1]
    accuracy
    
    # Increment the count
    count <- count + 1
    
    # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
    if(count2 == 5){
      break;
    }
  }
  
  # Record the end time for the current iteration
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and the selected hyperparameter value to the respective lists
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
  if(count2 == 5){
    break;
  }
}

# Combine the lists 'matris', 'matris2', 'matris3', 'matris4' into a single data frame 'Total'
Total <- cbind(matris, matris2, matris3, matris4)

# Write the 'Total' data frame to a CSV file "C:/Desktop/epsgridp.csv"
write.csv(Total, "C:/Desktop/epsgridp.csv")

# Record the end time of the script execution
end_time <- Sys.time()

# Calculate the elapsed time for the experiment
end_time - start_time

# Check the memory usage
mem_used()


#EPS grid Search b

# Initialize variables
count <- 1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Record the start time of the script execution
start_time <- Sys.time()

# The outer while loop with count2 controls the number of iterations
# The experiment will be performed 5 times (count2 <= 5)
while(count2 <= 5){
  # Record the start time for each iteration
  sTime <- Sys.time()
  
  # Reset the count and increment count2
  count <- 1
  count2 <- count2 + 1
  
  # Define the hyperparameter range 'hyper' as a sequence from 1 to 3
  hyper <- c(1:3)
  
  # The inner while loop with count controls the number of grid-based hyperparameter samples
  # In each iteration, it takes the next value from the 'hyper' range
  # And evaluates the EPS model's performance with that value
  while(count < count2){
    # Take the next value from the 'hyper' range as 'hyperOpt'
    hyperOpt <- sample(hyper, 1, replace = FALSE)
    
    # Load the dataset from the CSV file "C:/Users/Desktop/stack/processedDataset.csv"
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
    
    # Remove the first column of the dataset (assuming it contains row indices)
    dataGroup <- dataGroup[,-1]
    
    # Convert the dataset into an 'mldr' object named 'fileM'
    # Specify columns 51 to 72 as the label indices (y1 to y23)
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    
    # Split the 'fileM' dataset into training and test sets (80% training, 20% test)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    
    # Train the EPS model with the 'RF' base learner and the selected 'hyperOpt' value
    model <- eps(ds$train, "RF", m=10, b=hyperOpt, strategy="B") 
    
    # Make predictions on the test set using the trained model
    prediction <- predict(model, ds$test)
    
    # Evaluate the model's performance using the "bipartition" metric and obtain the labels
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    
    # Evaluate the model's performance using the "bipartition" metric
    thresres <- multilabel_evaluate(ds$test, prediction, "bipartition")
    thresres[3]
    
    # Store the accuracy value in the variable 'accuracy'
    accuracy <- result$multilabel[1]
    accuracy
    
    # Increment the count
    count <- count + 1
    
    # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
    if(count2 == 5){
      break;
    }
  }
  
  # Record the end time for the current iteration
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and the selected hyperparameter value to the respective lists
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
  if(count2 == 5){
    break;
  }
}

# Combine the lists 'matris', 'matris2', 'matris3', 'matris4' into a single data frame 'Total'
Total <- cbind(matris, matris2, matris3, matris4)

# Write the 'Total' data frame to a CSV file "C:/Desktop/epsgridb.csv"
write.csv(Total, "C:/Desktop/epsgridb.csv")

# Record the end time of the script execution
end_time <- Sys.time()

# Calculate the elapsed time for the experiment
end_time - start_time

# Check the memory usage
mem_used()


#EPS RANDOM Search b

# Initialize variables
count <- 1
count2 <- 2 
matris <- list("iter")
matris2 <- list("time")
matris3 <- list("accuracy")
matris4 <- list("hyperOpt")

# Record the start time of the script execution
start_time <- Sys.time()

# The outer while loop with count2 controls the number of iterations
# The experiment will be performed 5 times (count2 <= 5)
while(count2 <= 5){
  # Record the start time for each iteration
  sTime <- Sys.time()
  
  # Reset the count and increment count2
  count <- 1
  count2 <- count2 + 1
  
  # Define the hyperparameter range 'hyper' as a sequence from 1 to 3
  hyper <- c(1:3)
  
  # The inner while loop with count controls the number of random-based hyperparameter samples
  # In each iteration, it takes a random value from the 'hyper' range as 'hyperOpt'
  while(count < count2){
    # Take a random value from the 'hyper' range as 'hyperOpt'
    hyperOpt <- sample(hyper, 1, replace = TRUE)
    
    # Load the dataset from the CSV file "C:/Users/Desktop/stack/processedDataset.csv"
    dataGroup <- read.csv("C:/Users/Desktop/stack/processedDataset.csv")
    
    # Remove the first column of the dataset (assuming it contains row indices)
    dataGroup <- dataGroup[,-1]
    
    # Convert the dataset into an 'mldr' object named 'fileM'
    # Specify columns 51 to 72 as the label indices (y1 to y23)
    fileM <- mldr_from_dataframe(dataGroup, labelIndices = c(51:72), name = "fileM") 
    
    # Split the 'fileM' dataset into training and test sets (80% training, 20% test)
    ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
    
    # Train the EPS model with the 'RF' base learner and the selected 'hyperOpt' value
    model <- eps(ds$train, "RF", m=10, b=hyperOpt, strategy="B") 
    
    # Make predictions on the test set using the trained model
    prediction <- predict(model, ds$test)
    
    # Evaluate the model's performance using the "bipartition" metric and obtain the labels
    result <- multilabel_evaluate(ds$test, prediction, "bipartition", labels=TRUE)
    result$labels
    
    # Evaluate the model's performance using the "bipartition" metric
    thresres <- multilabel_evaluate(ds$test, prediction, "bipartition")
    thresres[3]
    
    # Store the accuracy value in the variable 'accuracy'
    accuracy <- result$multilabel[1]
    accuracy
    
    # Increment the count
    count <- count + 1
    
    # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
    if(count2 == 5){
      break;
    }
  }
  
  # Record the end time for the current iteration
  s2Time <- Sys.time()
  
  # Append the iteration number, elapsed time, accuracy, and the selected hyperparameter value to the respective lists
  matris <- list.append(matris, count2)
  matris2 <- list.append(matris2, s2Time - sTime)
  matris3 <- list.append(matris3, accuracy)
  matris4 <- list.append(matris4, hyperOpt)
  
  # Break the loop if count2 equals 5 to stop the experiment after 5 iterations
  if(count2 == 5){
    break;
  }
}

# Combine the lists 'matris', 'matris2', 'matris3', 'matris4' into a single data frame 'Total'
Total <- cbind(matris, matris2, matris3, matris4)

# Write the 'Total' data frame to a CSV file "C:/Desktop/epsrandomb.csv"
write.csv(Total, "C:/Desktop/epsrandomb.csv")

# Record the end time of the script execution
end_time <- Sys.time()

# Calculate the elapsed time for the experiment
end_time - start_time

# Check the memory usage
mem_used()


