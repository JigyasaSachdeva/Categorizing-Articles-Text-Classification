#IDS 575- Assignment 3
#Question 2
#Jigyasa Sachdeva
#UIN -664791188

#Repeating same steps as in Assignment 2 to load the train data 

#----------------------------------Train Data---------------------------------------------
require(Matrix)

# Change to the working directory where the data files are located.
# TODO: You should change the following ... to your working directory
setwd("/Users/jigyasasachdeva/Desktop/Third\ Sem/Statistics\ for\ Machine\ Learning/Assignment\ 2/hw2")

# Read all individual lines in a text file.
# m = the number of training examples
dataFile <- file("articles.train", "r")
dataLines <- readLines(dataFile)
m <- length(dataLines)
close(dataFile)

# Split every string element by tokenizing space and colon.
dataTokens = strsplit(dataLines, "[: ]")

# Extract every first token from each line as a vector of numbers, which is the class label.
Y = sapply(dataTokens, function(example) {as.numeric(example[1])})
table(Y)

# Extract the rest of tokens from each line as a list of matrices (one matrix for each line)
# where each row consists of two columns: (feature number, its occurrences)
X_list = lapply(dataTokens, function(example) {n = length(example) - 1; matrix(as.numeric(example[2:(n+1)]), ncol=2, byrow=T)})

# Add one column that indicates the example number at the left
X_list = mapply(cbind, x=1:length(X_list), y=X_list)

# Merge a list of different examples vertcially into a matrix
X_data = do.call('rbind', X_list)
dim(X_data)
class(X_data)

# Get a sparse data matrix X (rows: training exmaples, columns: # of occurrences for each of features)
bin = ifelse(X_data[,3] >=1, 1, 0)
X_binomial = sparseMatrix(x=bin, i=X_data[,1], j=X_data[,2])
X = sparseMatrix(x=X_data[,3], i=X_data[,1], j=X_data[,2])


#----------------------------------Test Data---------------------------------------------

#Repeating same steps as in Assignment 2 to load the test data 

# Read all individual lines in a text file.
# m = the number of training examples
dataFile_t <- file("articles.test", "r")
dataLines_t <- readLines(dataFile_t)
m_t <- length(dataLines_t)
close(dataFile_t)

# Split every string element by tokenizing space and colon.
dataTokens_t= strsplit(dataLines_t, "[: ]")

# Extract every first token from each line as a vector of numbers, which is the class label.
Y_test = sapply(dataTokens_t, function(example) {as.numeric(example[1])})
table(Y_test)

# Extract the rest of tokens from each line as a list of matrices (one matrix for each line)
# where each row consists of two columns: (feature number, its occurrences)
X_list_t = lapply(dataTokens_t, function(example) {n = length(example) - 1; matrix(as.numeric(example[2:(n+1)]), ncol=2, byrow=T)})

# Add one column that indicates the example number at the left
X_list_t = mapply(cbind, x=1:length(X_list_t), y=X_list_t)

# Merge a list of different examples vertcially into a matrix
X_data_t = do.call('rbind', X_list_t)
dim(X_data_t)
class(X_data_t)

# Get a sparse data matrix X (rows: training exmaples, columns: # of occurrences for each of features)
X_data_binomial = ifelse(X_data_t[,3] >=1, 1, 0)
X_test_binomial = sparseMatrix(x=X_data_binomial, i=X_data_t[,1], j=X_data_t[,2])
X_test = sparseMatrix(x=X_data_t[,3], i=X_data_t[,1], j=X_data_t[,2])


#--------------------------------------------------------------------------------------------------
#Question 2


library(e1071)
library(SparseM)
library(caret)
library(dplyr)


#Prior probability
#p(y = k)
freq <- table(Y)
priori <- prop.table(freq)
priori

#Conditional probability
#p(x_j / y= k)
#Selecting rows which belong to respective categories
art_1 <- which(Y==1)
art_2 <- which(Y==2)
art_3 <- which(Y==3)
art_4 <- which(Y==4)

#Articles for Category 1
#Selecting the part of matrix for each category:
art_X1 <- X_binomial[art_1,]
#Calculating sum of existence (1/0) of each word given the category: 
c1 = colSums (art_X1, na.rm = FALSE, dims = 1)
c1 = c1/1000

#Articles for Category 2
#Selecting the part of matrix for each category:
art_X2 <- X_binomial[art_2,]
#Calculating sum of existence (1/0) of each word given the category: 
c2 = colSums (art_X2, na.rm = FALSE, dims = 1)
c2 = c2/1000

#Articles for Category 3
#Selecting the part of matrix for each category:
art_X3 <- X_binomial[art_3,]
#Calculating sum of existence (1/0) of each word given the category: 
c3 = colSums (art_X3, na.rm = FALSE, dims = 1)
c3 = c3/1000

#Articles for Category 4
#Selecting the part of matrix for each category:
art_X4 <- X_binomial[art_4,]
#Calculating sum of existence (1/0) of each word given the category: 
c4 = colSums (art_X4, na.rm = FALSE, dims = 1)
c4 = c4/1000

#Displaying conditional probabilities: 
c1
c2
c3
c4

#Naive Bayes Model:
existence_per_article = apply(X_binomial, 1, function(x) {return(which(x==1))})
str(existence_per_article)
rm(r)
result <- vector()
for(i in 1:4000)
{
  ind = unlist(existence_per_article[i])
  p1 = prod(c1[ind])
  p2 = prod(c2[ind])
  p3 = prod(c3[ind])
  p4 = prod(c4[ind])
  r = which.max(c(p1, p2, p3, p4))
  result = c(result, r)
}

result
classes = as.factor(result)

#Classification on training data:
confusionMatrix(as.factor(Y), classes)


#----------------------------------------------------------------------------------------

#(c)

#Laplace smoothening
#Replacing each 0 value with 1/4000 in all c1, c2, c3, c4 vectors
#Replacing with 0.00025
c1_l <- ifelse(c1 == 0, 0.00025,c1)
c2_l <- ifelse(c2 == 0, 0.00025,c2)
c3_l <- ifelse(c3 == 0, 0.00025,c3)
c4_l <- ifelse(c4 == 0, 0.00025,c4)

#Naive bayes model with laplace smoothening on train data 
existence_per_article = apply(X_binomial, 1, function(x) {return(which(x==1))})
rm(r)
result <- vector()
for(i in 1:4000)
{
  ind = unlist(existence_per_article[i])
  p1 = prod(c1_l[ind])
  p2 = prod(c2_l[ind])
  p3 = prod(c3_l[ind])
  p4 = prod(c4_l[ind])
  r = which.max(c(p1, p2, p3, p4))
  result = c(result, r)
}

result
classes = as.factor(result)
#Classification on training data:
confusionMatrix(as.factor(Y), classes)


#Naive bayes model with laplace smoothening on test data: 
existence_per_article_test = apply(X_test_binomial, 1, function(x) {return(which(x==1))})
test_result <- vector()
for(i in 1:2400)
{
  ind = unlist(existence_per_article_test[i])
  p1 = prod(c1_l[ind], na.rm = T)
  p2 = prod(c2_l[ind], na.rm = T)
  p3 = prod(c3_l[ind], na.rm = T)
  p4 = prod(c4_l[ind], na.rm = T)
  r = which.max(c(p1, p2, p3, p4))
  test_result = c(test_result, r)
}

test_result
test_classes = as.factor(test_result)
#Prediction results on test data:
confusionMatrix(as.factor(Y_test), test_classes)


#-------------------------------------------------------------------------------------------
#(d)
#Multinomial Naive bayes
#Raising the probability to the power of frequency
existence_per_article = apply(X_binomial, 1, function(x) {return(which(x==1))})
result <- vector()
for(i in 1:4000)
{
  ind = unlist(existence_per_article[i])
  f = X[i,existence_per_article[[i]]]
  
  #Class 1
  a1 = c1_l[ind]^f
  p1 = prod(a1, na.rm = T)
  
  #Class 2
  a2 = c2_l[ind]^f
  p2 = prod(a2, na.rm = T)
  
  #Class 3
  a3 = c3_l[ind]^f
  p3 = prod(a3, na.rm = T)
  
  #Class 4
  a4 = c4_l[ind]^f
  p4 = prod(a4, na.rm = T)

  #Result:
  r = which.max(c(p1, p2, p3, p4))
  result = c(result, r)
}

result
classes = as.factor(result)
#Classification on training data:
confusionMatrix(as.factor(Y), classes)



#Test data:
existence_per_article_test = apply(X_test_binomial, 1, function(x) {return(which(x==1))})
rm(r)
test_result <- vector()
for(i in 1:2400)
{
  ind = unlist(existence_per_article_test[i])
  f = X_test[i,existence_per_article_test[[i]]]
  ifelse(f == 0, 0.00025,f)
  
  #Class 1
  a1 = c1_l[ind]^f
  p1 = prod(a1, na.rm = T)
  
  #Class 2
  a2 = c2_l[ind]^f
  p2 = prod(a2, na.rm = T)
  
  #Class 3
  a3 = c3_l[ind]^f
  p3 = prod(a3, na.rm = T)
  
  #Class 4
  a4 = c4_l[ind]^f
  p4 = prod(a4, na.rm = T)
  
  #Result:
  r = which.max(c(p1, p2, p3, p4))
  test_result = c(test_result, r)
}

test_result
classes = as.factor(test_result)
#Classification on training data:
confusionMatrix(as.factor(Y_test), classes)



