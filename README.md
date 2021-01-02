# Categorizing-Articles
Text classification problem for information retrieval and natural language processing. The task is to classify eahc text article into one of the predefined classes/catefories: operating systems, vehicles, sports, politics. 

SVM: 
- Training 4 different hard-margin linear classifiers and predicting new text articles on the basis of arg max probabilities attained from linear SVM
- Hyper parameter tuning of cost parameter for each binary classifier and measuring the overall classification error
- Training a soft-margin classifier on the entire training set and comparing results with hard margin classifier
- Normalizing feature vectors of each article to measure the soft margin validation error rate 

Naive Bayes: 
- Using MLE Recipe, trained a Bernouli Naive Bayes model using conditional probabilities and validated the model on test data
- Implemented Laplace Smoothening on the same Naive Bayes model to prevent null probabilities
- Executed multinomial naive bayes model to compare and contrast results with the previous steps
