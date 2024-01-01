# A heuristic technique for hyperpameter tuning of coding experience prediction using Stack Overflow posts
<p align="justify">In this study, a book recommendation system based on multi-label classification is proposed by leveraging the Stack Overflow corpus. To that end, an up-to-date Stack Overflow data set is analyzed to devise a labeling function comprising 21 programming languages. The specified books are presented to the users according to the manual examination performed for three development levels. For the five different classifiers, the proposed hyperparameter optimization method achieved a higher accuracy than the traditional ones. In the comparison, the proposed method claims a competitive time and memory consumption for traditional hyperparameter optimization methods along with a stable success in changing hyperparameter values.</p>

# Background
<p align="center">
  <img src="https://github.com/fatmaaltinsoy/a-heuristic-technique-for-hyperpameter-tuning/blob/main/background.png" alt="resim açıklaması">
</p>
The study comprises three phases:
<p align="justify"> 1. The SO dataset is processed to create a word matrix, enabling the application of sophisticated functions for the creation of classification labels. These labels encompass both the programming language type and the level of programming experience. Each word is considered a unique feature, aggregated from the question and answer inputs.</p>
<p align="justify"> 2. A tuning operation is performed, employing several methods, namely grid search, random search, Bayesian optimization, and Nelder-Mead, on both single-label and multi-label classification algorithms (SVM, Random Forest, GBM, EBR, and EPS).</p>
<p align="justify"> 3. The coding experience level is predicted based on the training data, followed by the proposal of a suitable programming book.</p>

# Dataset
<p align="justify"> In this case study, we utilized the Stack Overflow corpus, which regularly provides question-answer inputs to researchers for public use. The corpus consists of two types of data: questions and tags.</p>
<p align="justify"> 1. Questions.csv (https://www.kaggle.com/stackoverflow/stacksample) </p>
<p align="justify"> 2. Tags.csv (https://www.kaggle.com/stackoverflow/stacksample) </p>
<p align="justify"> The former includes questions covering over twenty programming languages, with a total of 1,048,575 instances. It is very difficult to load all the instances to the RAM. Instead you can download question1.csv to make a small experiment.</p>

# Labeling
<p align="justify"> A function is written to determine whether the question dataset contains JavaScript, SQL, Java, C#, Python, C++, C, PHP, Ruby, Swift, Objective-C, VB.NET, Perl, Bash, CSS, Scala, HTML, Lua, Haskell, Markdown, and R programming languages. The labeling process involved assigning a value of 1 to indicate that a question included a keyword related to a programming language, while a value of 0 was assigned if the question was not related. Both the title and body of the questions were utilized to create labels indicating the coding experience of the users. When the code.R file is executed, after the word vector operations on the first 150 questions in the question dataset, <b>a processed dataset </b> is obtained containing the counts of words in the body field, the total number of words and characters, y1-y21 representing JavaScript, SQL, Java, C#, Python, C++, C, PHP, Ruby, Swift, Objective-C, VB.NET, Perl, Bash, CSS, Scala, HTML, Lua, Haskell, Markdown, and R programming languages, y22 denoting experience 1 (beginner), 2 (intermediate), and 3 (advanced), and y23 representing the recommended book ID.<b> For labeling operations, you need to run the labeling.R file first and then the code.R file. Detailed explanations of the code are provided within the R files.</b></p>

# Hyperparameter Optimization
<p align="justify"> processedDataset.csv is essential for conducting hyperparameter optimization, which involves applying grid search, random search, Bayesian optimization, Nelder-Mead, and the proposed method on various algorithms including GBM, random forest, SVM, EBR, and EPS. The goal of this optimization is to enhance the accuracy of programming experience prediction. The accuracy results obtained from these experiments are accompanied by time and memory consumption statistics. In the files randomForest.R, gbm.R, svm.R, eps.R, and ebr.R, all hyperparameter optimization processes and codes are explained in detail.</p>

# Installation
Step 1. In order to run the code in code.R, it is necessary to load these libraries first.

<p align="justify"><b>rlist:</b> The "rlist" package provides functions to work with lists in R. It includes various utility functions for manipulating and accessing elements in lists, making it easier to work with complex data structures.</p>

<p align="justify"><b>tm:</b> The "tm" (Text Mining) package is designed for text mining and natural language processing tasks in R. It provides functions for preprocessing text data, such as removing punctuation, stopwords, and stemming, and allows users to create term-document matrices for further analysis.</p>

<p align="justify"><b>plyr:</b> The "plyr" package is used for data manipulation and transformation tasks. It provides a set of functions for splitting, applying, and combining data in various ways. The package's main functions are ddply, dlply, and adply, which help to perform operations on data frames and return the results in an organized manner.</p>

<p align="justify"><b>caret:</b> The "caret" (Classification And Regression Training) package is widely used for machine learning tasks in R. It provides a consistent interface for building and comparing different models, handling data preprocessing steps, and tuning hyperparameters. It is especially useful for classification and regression tasks.</p>

<p align="justify"><b>caTools:</b> The "caTools" package in R is a collection of utility functions that provide useful tools for various data manipulation and preprocessing tasks. It offers functions to split datasets, standardize data, create dummy variables, and compute logit transformations, among other functionalities. These functions are particularly valuable for preparing data for machine learning models.</p>

```sh
install.packages("rlist")
install.packages("tm")
install.packages("plyr")
install.packages("caret")
install.packages("caTools")
```
<p align="justify"> Step 2. For hyperparameter optimization, the following libraries need to be installed. The <b>caret library</b> is used for grid search and random search.</p>

<p align="justify"><b>MlBayesOpt:</b> MlBayesOpt is a package for performing Bayesian optimization, which is a powerful optimization technique that can be used to find the optimal values of hyperparameters for machine learning models. It uses a probabilistic model to efficiently explore the hyperparameter space and find the configuration that maximizes the performance of the model.

<p align="justify"><b>nloptr:</b> nloptr is a package for nonlinear optimization. It provides a collection of optimization algorithms that can be used to find the minimum or maximum of a nonlinear objective function, subject to optional constraints. This package is useful for various optimization problems, including hyperparameter optimization for machine learning models.

```sh
install.packages("MlBayesOpt")
install.packages("nloptr")
```
Step 3. To use the random forest, GBM, SVM, EPS, and EBR algorithms, the following libraries need to be installed.

<p align="justify"><b>randomForest:</b> An R package for the Random Forest algorithm. Random Forest is an ensemble method that combines multiple decision trees to achieve high-performance classification and regression tasks.

<p align="justify"><b>gbm:</b> An R package for Gradient Boosting Machine (GBM) algorithm. GBM combines weak learners (often decision trees) to create a stronger learner and provides effective results for classification and regression problems.

<p align="justify"><b>e1071:</b> This R package provides functions and tools for the Support Vector Machine (SVM) algorithm. SVM is a powerful machine learning algorithm used for classification and regression tasks.

<p align="justify"><b>utiml:</b> An R package for multi-label classification and label space operations. This package facilitates the processing and evaluation of multi-label datasets.

<p align="justify"><b>mldr:</b> This R package is used to represent and process multi-label datasets. The mldr package allows the creation of label spaces containing datasets for multi-label classification.

```sh
install.packages("randomForest")
install.packages("gbm")
install.packages("e1071")
install.packages("utiml")
install.packages("mldr")
```
# Execution Order of the Files
For the given files in this repository, follow the sequence of execution mentioned below:
1. Run the `labeling.R` file first. This will create labels for your data.
2. Next, run the `code.R` file. This will process the data and prepare it for model fitting.
3. After this, you can proceed with model fitting. Run the `gbm.R`, `svm.R`, `EBR.R`, `EPS.R`, `randomForest.R` files in any order as per your requirement. These files each implement different machine learning models and their execution order is not dependent on each other.

In each file, hyperparameter optimization is conducted utilizing several methods, namely grid search, random search, Bayesian optimization, and Nelder-Mead. The goal is to enhance the accuracy of the programming experience prediction. The results obtained from these optimizations can be compared to determine the most efficient model and hyperparameters.
