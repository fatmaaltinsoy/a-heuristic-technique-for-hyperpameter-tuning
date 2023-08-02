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
<p align="justify"> processedDataset.csv is essential for conducting hyperparameter optimization, which involves applying grid search, random search, Bayesian optimization, Nelder-Mead, and the proposed method on various algorithms including GBM, random forest, SVM, EBR, and EPS. The goal of this optimization is to enhance the accuracy of programming experience prediction. The accuracy results obtained from these experiments are accompanied by time and memory consumption statistics. In the files RandomForest.R, GBM.R, SVM.R, EPS.R, and EBR.R, all hyperparameter optimization processes and codes are explained in detail.</p>
