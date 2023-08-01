# A heuristic technique for hyperpameter tuning of coding experience prediction using Stack Overflow posts
<p align="justify">In this study, a book recommendation system based on multi-label classification is proposed by leveraging the Stack Overflow corpus. To that end, an up-to-date Stack Overflow data set is analyzed to devise a labeling function comprising 21 programming languages. The specified books are presented to the users according to the manual examination performed for three development levels. For the five different classifiers, the proposed hyperparameter optimization method achieved a higher accuracy than the traditional ones. In the comparison, the proposed method claims a competitive time and memory consumption for traditional hyperparameter optimization methods along with a stable success in changing hyperparameter values.</p>

# Background
<p align="center">
  <img src="https://github.com/fatmaaltinsoy/a-heuristic-technique-for-hyperpameter-tuning/blob/main/background.png" alt="resim açıklaması">
</p>
The study comprises three phases:
1. The SO dataset is processed to create a word matrix, enabling the application of sophisticated functions for the creation of classification labels. These labels encompass both the programming language type and the level of programming experience. Each word is considered a unique feature, aggregated from the question and answer inputs.
2. A tuning operation is performed, employing several methods, namely grid search, random search, Bayesian optimization, and Nelder-Mead, on both single-label and multi-label classification algorithms (SVM, Random Forest, GBM, EBR, and EPS). 
3.The coding experience level is predicted based on the training data, followed by the proposal of a suitable programming book.
