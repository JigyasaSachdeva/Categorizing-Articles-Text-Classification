#Jigyasa Sachdeva
#UIN- 664791188
#IDS 575- Assignment 2: Question 5

#************************************************************************************
#Given code: 

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
X_list[4000]

# Merge a list of different examples vertcially into a matrix
X_data = do.call('rbind', X_list)
dim(X_data)
class(X_data)

# Get a sparse data matrix X (rows: training exmaples, columns: # of occurrences for each of features)
X = sparseMatrix(x=X_data[,3], i=X_data[,1], j=X_data[,2])


#********************************************************************************************
#(a)

#Loading test data into R the same way train data was imported: 

dataTestFile <- file("articles.test", "r")
dataTestLines <- readLines(dataTestFile)
length(dataTestLines)
close(dataTestFile)
dataTestTokens = strsplit(dataTestLines, "[: ]")
Y_Test = sapply(dataTestTokens, function(example) {as.numeric(example[1])})
table(Y_Test)
X_Test_list = lapply(dataTestTokens, function(example) {n = length(example) - 1; matrix(as.numeric(example[2:(n+1)]), ncol=2, byrow=T)})
X_Test_list = mapply(cbind, x=1:length(X_Test_list), y=X_Test_list)
X_Test_data = do.call('rbind', X_Test_list)
X_Test = sparseMatrix(x=X_Test_data[,3], i=X_Test_data[,1], j=X_Test_data[,2])

#Training data:
#Input features:
X
X_Test


#********************************************************************************************
#(b)

#Loading required libraries:
library(Matrix)
library(e1071) #For SVM
library(caret) #For Confusion Matrix
options(scipen=99)


#SVM model classifying articles as 'Operating Systems' or 'Non Operating Systems' 

#1: Operating Systems, -1: Non Operating Systems
Y_Model1 <- ifelse(Y==1, 1, -1)
Y_Model1 <- as.factor(Y_Model1)
Y_Test_Model1 <- ifelse(Y_Test==1, 1, -1)
Y_Test_Model1 <- as.factor(Y_Test_Model1)

#Building model1
Model_1 <- svm(X, Y_Model1, kernel = "linear", probability = TRUE)

#Predicting on train data
pred_1 <-predict(Model_1, X, probability = T)
predtrain1 <- as.matrix(attr(pred_1, "probabilities"))[,'1']
confusionMatrix(pred_1, Y_Model1, positive = '1')
Model1_Train_error <- 1- confusionMatrix(pred_1, Y_Model1, positive = '1')$overall['Accuracy']

#Predicting on test  data
#For same number of words in train and test data: 
pred_Test_1 <-predict(Model_1, X_Test[,1:51949], probability = T)
confusionMatrix(pred_Test_1, Y_Test_Model1, positive = '1')
Model1_Test_error <- 1- confusionMatrix(pred_Test_1, Y_Test_Model1, positive = '1')$overall['Accuracy']
Model1_Test_error #0.0425

Model1 <- c(Model1_Train_error, Model1_Test_error)



#SVM model classifying articles as 'Vehicles' or 'Non Vehicles' 

#1: Vehicles, -1: Non Vehicles
Y_Model2 <- ifelse(Y==2, 1, -1)
Y_Model2 <- as.factor(Y_Model2)
Y_Test_Model2 <- ifelse(Y_Test==2, 1, -1)
Y_Test_Model2 <- as.factor(Y_Test_Model2)

#Building model2
Model_2 <- svm(X, Y_Model2, kernel = "linear", probability = TRUE)

#Predicting on train data
pred_2 <-predict(Model_2, X, probability = T)
predtrain2 <- as.matrix(attr(pred_2, "probabilities"))[,'1']
confusionMatrix(pred_2, Y_Model2, positive = '1')
Model2_Train_error <- 1- confusionMatrix(pred_2, Y_Model2, positive = '1')$overall['Accuracy']

#Predicting on test  data
#For same number of words in train and test data: 
pred_Test_2 <-predict(Model_2, X_Test[,1:51949], probability = T)
confusionMatrix(pred_Test_2, Y_Test_Model2, positive = '1')
Model2_Test_error <- 1- confusionMatrix(pred_Test_2, Y_Test_Model2, positive = '1')$overall['Accuracy']
Model2_Test_error #0.0625

Model2 <- c(Model2_Train_error, Model2_Test_error)


#SVM model classifying articles as 'Sports' or 'Non Sports' 

#1: Sports, -1: Non Sports
Y_Model3 <- ifelse(Y==3, 1, -1)
Y_Model3 <- as.factor(Y_Model3)
Y_Test_Model3 <- ifelse(Y_Test==3, 1, -1)
Y_Test_Model3 <- as.factor(Y_Test_Model3)

#Building model3
Model_3 <- svm(X, Y_Model3, kernel = "linear", probability = TRUE)

#Predicting on train data
pred_3 <-predict(Model_3, X, probability = T)
predtrain3 <- as.matrix(attr(pred_3, "probabilities"))[,'1']
confusionMatrix(pred_3, Y_Model3, positive = '1')
Model3_Train_error <- 1- confusionMatrix(pred_3, Y_Model3, positive = '1')$overall['Accuracy']

#Predicting on test  data
#For same number of words in train and test data: 
pred_Test_3 <-predict(Model_3, X_Test[,1:51949], probability = T)
confusionMatrix(pred_Test_3, Y_Test_Model3, positive = '1')
Model3_Test_error <- 1- confusionMatrix(pred_Test_3, Y_Test_Model3, positive = '1')$overall['Accuracy']
Model3_Test_error #0.04791667

Model3 <- c(Model3_Train_error, Model3_Test_error)



#SVM model classifying articles as 'Politics' or 'Non Politics' 

#1: Politics, -1: Non Politics
Y_Model4 <- ifelse(Y==4, 1, -1)
Y_Model4 <- as.factor(Y_Model4)
Y_Test_Model4 <- ifelse(Y_Test==4, 1, -1)
Y_Test_Model4 <- as.factor(Y_Test_Model4)

#Building model4
Model_4 <- svm(X, Y_Model4, kernel = "linear", probability = TRUE)

#Predicting on train data
pred_4 <-predict(Model_4, X, probability = T)
predtrain4 <- as.matrix(attr(pred_4, "probabilities"))[,'1']
confusionMatrix(pred_4, Y_Model4, positive = '1')
Model4_Train_error <- 1- confusionMatrix(pred_4, Y_Model4, positive = '1')$overall['Accuracy']

#Predicting on test  data
#For same number of words in train and test data: 
pred_Test_4 <-predict(Model_4, X_Test[,1:51949], probability = T)
confusionMatrix(pred_Test_4, Y_Test_Model4, positive = '1')
Model4_Test_error <- 1- confusionMatrix(pred_Test_4, Y_Test_Model4, positive = '1')$overall['Accuracy']
Model4_Test_error #0.05833333

Model4 <- c(Model4_Train_error, Model4_Test_error)

#Error:
error_df<- rbind(Model1, Model2, Model3, Model4)
colnames(error_df) <- c('Training error', "Test Error")
error_df


#Combining all four classifiers together
Final_prediction <- data.frame(pred_Test_1, pred_Test_2, pred_Test_3, pred_Test_4)
max_label <- max.col(Final_prediction)
max_label <- as.factor(max_label)
Y_Test <- as.factor(Y_Test)
confusionMatrix(max_label, Y_Test)
#Overall classification


#*************************************************************************************************
#(c)

#Splitting all the training data in train and validation
#Index: 
set.seed(123)
index <- sample(seq_len(nrow(X)), size = floor(0.75*nrow(X)))
#Splitting X:
xtrain <- X[index, ]
xval <- X[-index,]
#Splitting respective Y for all 4 classifiers: 
Y1_train <- Y_Model1[index]
Y1_val <- Y_Model1[-index]
Y2_train <- Y_Model2[index]
Y2_val <- Y_Model2[-index]
Y3_train <- Y_Model3[index]
Y3_val <- Y_Model3[-index]
Y4_train <- Y_Model4[index]
Y4_val <- Y_Model4[-index]
#Splitting Y for original label:
Y_train <- Y[index]
Y_val <- Y[-index]


train_err1 = 0
valid_err1 = 0
train_err2 = 0
valid_err2 = 0
train_err3 = 0
valid_err3 = 0
train_err4 = 0
valid_err4 = 0
Overall_t_err = 0
Overall_v_err = 0

cost_val <- c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512)

for(i in 1:length(cost_val))
{
  #Binary classifier 1: 
  svm1 <- svm(xtrain, factor(Y1_train), cost =cost_val[i], probability=TRUE, kernel = "linear")
  p_train1 <- predict(svm1, xtrain, probability = TRUE)
  p_val1 <- predict(svm1, xval, probability = TRUE)
  preds_t_1 <- as.matrix(attr(p_train1, "probabilities"))[,'1']
  preds_v_1 <- as.matrix(attr(p_val1, "probabilities"))[,'1']
  c1 <- confusionMatrix(factor(p_train1), factor(Y1_train))
  c2 <- confusionMatrix(factor(p_val1), factor(Y1_val))
  train_err1 <- cbind(train_err1, 1- c1$overall[1])
  valid_err1 <- cbind(valid_err1, 1 - c2$overall[1])

  #Binary classifier 2
  svm2 <- svm(xtrain, factor(Y2_train), cost = cost_val[i], probability=TRUE, kernel = "linear")
  p_train2 <- predict(svm2, xtrain, probability = TRUE)
  p_val2 <- predict(svm2, xval, probability = TRUE)
  preds_t_2 <- as.matrix(attr(p_train1, "probabilities"))[,'1']
  preds_v_2 <- as.matrix(attr(p_val1, "probabilities"))[,'1']
  c1 <- confusionMatrix(factor(p_train2), factor(Y2_train))
  c2 <- confusionMatrix(factor(p_val2), factor(Y2_val))
  train_err2 <- cbind(train_err2, 1- c1$overall[1])
  valid_err2 <- cbind(valid_err2, 1 - c2$overall[1])
  
  #Binary classifier 3
  svm3 <- svm(xtrain, factor(Y3_train), cost = cost_val[i], probability=TRUE, kernel = 'linear')
  p_train3 <- predict(svm3, xtrain, probability = TRUE)
  p_val3 <- predict(svm3, xval, probability = TRUE)
  preds_t_3 <- as.matrix(attr(p_train1, "probabilities"))[,'1']
  preds_v_3 <- as.matrix(attr(p_val1, "probabilities"))[,'1']
  c1 <- confusionMatrix(factor(p_train3), factor(Y3_train))
  c2 <- confusionMatrix(factor(p_val3), factor(Y3_val))
  train_err3 <- cbind(train_err3, 1- c1$overall[1])
  valid_err3 <- cbind(valid_err3, 1 - c2$overall[1])

  #Binary classifier 4  
  svm4 <- svm(xtrain, factor(Y4_train), cost = cost_val[i], probability=TRUE, kernel = 'linear')
  p_train4 <- predict(svm4, xtrain, probability = TRUE)
  p_val4 <- predict(svm4, xval, probability = TRUE)
  preds_t_4 <- as.matrix(attr(p_train1, "probabilities"))[,'1']
  preds_v_4 <- as.matrix(attr(p_val1, "probabilities"))[,'1']
  c1 <- confusionMatrix(factor(p_train4), factor(Y4_train))
  c2 <- confusionMatrix(factor(p_val4), factor(Y4_val))
  train_err4 <- cbind(train_err4, 1- c1$overall[1])
  valid_err4 <- cbind(valid_err4, 1 - c2$overall[1])

  #training data probabilities of each model  
  train_eval <- data.frame(p_train1, p_train2, p_train3, p_train4)
  #validation data probabilities of each model 
  valid_eval <- data.frame(p_val1, p_val2, p_val3, p_val4)
  
  #class prediction for multi-class classification
  #training data
  final_teval <- max.col(train_eval)
  #validation data
  final_veval <- max.col(valid_eval)
  
  #confusion matrices and overall error, accuracy
  c_train <- confusionMatrix(factor(final_teval), factor(Y_train))
  c_val <- confusionMatrix(factor(final_veval), factor(Y_val))
  Overall_t_err <- cbind(Overall_t_err, 1 - c_train$overall[1])
  Overall_v_err <- cbind(Overall_v_err, 1 - c_val$overall[1])

}

#Training error:
training <- rbind(train_err1,train_err2, train_err3, train_err4, Overall_t_err)
training <- training[,-1]
colnames(training) <- cost_val
rownames(training) <- c("Model1", "Model2", "Model3", "Model4", "Overall")
training

#Validation error:
validation <- rbind(valid_err1,valid_err2, valid_err3, valid_err4, Overall_v_err)
validation <- validation[,-1]
colnames(validation) <- cost_val
rownames(validation) <- c("Model1", "Model2", "Model3", "Model4", "Overall")
validation


cost_log = log2(cost_val)
Overall_t_err <- Overall_t_err[-1]
Overall_v_err <- Overall_v_err[-1]
x= c("train", "validation")


#Plotting training, validation error for each cost value in 4 classifiers: 
library(ggplot2)

#Model 1
ggplot()+
  geom_line(mapping = aes(x = cost_log, y = training[1,], color = "Training"))+
  geom_line(mapping = aes(x = cost_log, y = validation[1,], color = "Validation"))+
  xlab(label = 'Log Cost')+
  ylab(label = 'Error')+
  ylim(0, 0.1)+
  ggtitle('Error comparison on Model 1')


#Model 2
ggplot()+
  geom_line(mapping = aes(x = cost_log, y = training[2,], color = "Training"))+
  geom_line(mapping = aes(x = cost_log, y = validation[2,], color = "Validation"))+
  xlab(label = 'Log Cost')+
  ylab(label = 'Error')+
  ylim(0,0.1)+
  ggtitle('Error comparison on Model 2')

#Model 3
ggplot()+
  geom_line(mapping = aes(x = cost_log, y = training[3,], color = "Training"))+
  geom_line(mapping = aes(x = cost_log, y = validation[3,], color = "Validation"))+
  xlab(label = 'Log Cost')+
  ylab(label = 'Error')+
  ylim(0,0.1)+
  ggtitle('Error comparison on Model 3')

#Model 4
ggplot()+
  geom_line(mapping = aes(x = cost_log, y = training[4,], color = "Training"))+
  geom_line(mapping = aes(x = cost_log, y = validation[4,], color = "Validation"))+
  xlab(label = 'Log Cost')+
  ylab(label = 'Error')+
  ylim(0,0.1)+
  ggtitle('Error comparison on Model 4')


#Overall plot
ggplot()+
  geom_line(mapping = aes(x = cost_log, y = Overall_t_err, color = "Training"))+
  geom_line(mapping = aes(x = cost_log, y = Overall_v_err, color = "Validation"))+
  xlab(label = 'Log Cost')+
  ylab(label = 'Error')+
  ylim(0,0.15)+
  ggtitle('Error comparison on Overall Model')


#*******************************************************************************

#(d)

#Efficient Cost = 0.125
#Running the soft margin binary classifiers on the entire train and test data

#Model 1
svm1 <- svm(X, Y_Model1, cost = 0.125, probability=TRUE, kernel = 'linear')
p_train1 <- predict(svm1, X, probability = TRUE)
p_val1 <- predict(svm1, X_Test[,1:51949], probability = TRUE)
predval1 <- as.matrix(attr(p_val1, "probabilities"))[,'1']
c1 <- confusionMatrix(factor(p_train1), factor(Y_Model1), positive = '1')
c2 <- confusionMatrix(factor(p_val1), factor(Y_test_Model1), positive = '1')
t_err1 <- 1- c1$overall[1] #0.00025
v_err1 <- 1 - c2$overall[1] #0.0375

#Model 2
svm2 <- svm(X, Y_Model2, cost = 0.125, probability=TRUE, kernel = 'linear')
p_train2 <- predict(svm2, X, probability = TRUE)
p_val2 <- predict(svm2, X_Test[,1:51949], probability = TRUE)
predval2 <- as.matrix(attr(p_val2, "probabilities"))[,'1']
c1 <- confusionMatrix(factor(p_train2), factor(Y_Model2), positive = '1')
c2 <- confusionMatrix(factor(p_val2), factor(Y_test_Model2), positive = '1')
t_err2 <- 1- c1$overall[1] #0.001
v_err2 <- 1 - c2$overall[1] #0.05541667

#Model 3
svm3 <- svm(X, Y_Model3, cost = 0.125, probability=TRUE, kernel = 'linear')
p_train3 <- predict(svm3, X, probability = TRUE)
p_val3 <- predict(svm3, X_Test[,1:51949], probability = TRUE)
predval3 <- as.matrix(attr(p_val3, "probabilities"))[,'1']
c1 <- confusionMatrix(factor(p_train3), factor(Y_Model3), positive = '1')
c2 <- confusionMatrix(factor(p_val3), factor(Y_Test_Model3), positive = '1')
t_err3 <- 1- c1$overall[1] #0.0005
v_err3 <- 1 - c2$overall[1] #0.03458333 

#Model 4
svm4 <- svm(X, Y_Model4, cost = 0.125, probability=TRUE, kernel = 'linear')
p_train4 <- predict(svm4, X, probability = TRUE)
p_val4 <- predict(svm4, X_Test[,1:51949], probability = TRUE)
predval4 <- as.matrix(attr(p_val4, "probabilities"))[,'1']
c1 <- confusionMatrix(factor(p_train4), factor(Y_Model4), positive = '1')
c2 <- confusionMatrix(factor(p_val4), factor(Y_Test_Model4), positive = '1')
t_err4 <- 1- c1$overall[1] #0.00025
v_err4 <- 1 - c2$overall[1] #0.05791667

a <- c(t_err1, t_err2, t_err3, t_err4)
b <- c(v_err1, v_err2, v_err3, v_err4)
df <- cbind(a,b)
colnames(df) <- c('Train', 'Test')
rownames(df) <- c('Model1', 'Model2', 'Model3', 'Model4')
df

#Overall prediction on test data
Final_prediction <- data.frame(p_val1, p_val2, p_val3, p_val4)
max_label <- max.col(Final_prediction)
max_label <- as.factor(max_label)
Y_Test <- as.factor(Y_Test)
confusionMatrix(max_label, Y_Test)


#************************************************************************************
#(e)

install.packages("wordspace")
library(wordspace)
normalizing_data <- normalize.rows(X, method="euclidean", p=2)

#Training the Soft Margin classifier 1 (OS) with Normalized feature space and determined cost: 
svm_normalized_1 <- svm(normalizing_data, Y_Model1, kernel = "linear", 
                        cost=0.125, probability = TRUE) 

p_train1 <- predict(svm_normalized_1, normalizing_data, probability = TRUE)
p_val1 <- predict(svm_normalized_1, X_Test[,1:51949], probability = TRUE)
predval1 <- as.matrix(attr(p_val1, "probabilities"))[,'1']
c1 <- confusionMatrix(factor(p_train1), factor(Y_Model1), positive = '1')
c2 <- confusionMatrix(factor(p_val1), factor(Y_test_Model1), positive = '1')
t_err1 <- 1- c1$overall[1] #0
v_err1 <- 1 - c2$overall[1] #0.03875


#Training the Soft Margin classifier 2 (Vehicales) with Normalized feature space and determined cost:
svm_normalized_2 <- svm(normalizing_data, Y_Model2, kernel = "linear", 
                        cost=128, probability = TRUE) 
p_train2 <- predict(svm_normalized_2, normalizing_data, probability = TRUE)
p_val2 <- predict(svm_normalized_2, X_Test[,1:51949], probability = TRUE)
predval2 <- as.matrix(attr(p_val2, "probabilities"))[,'1']
c1 <- confusionMatrix(factor(p_train2), factor(Y_Model2), positive = '1')
c2 <- confusionMatrix(factor(p_val2), factor(Y_test_Model2), positive = '1')
t_err2 <- 1- c1$overall[1] #0
v_err2 <- 1 - c2$overall[1] #0.04333


#Training the Soft Margin classifier 2 (Vehicales) with Normalized feature space and determined cost:
svm_normalized_3 <- svm(normalizing_data, Y_Model3, kernel = "linear", 
                        cost=128, probability = TRUE) 
p_train3 <- predict(svm_normalized_3, normalizing_data, probability = TRUE)
p_val3 <- predict(svm_normalized_3, X_Test[,1:51949], probability = TRUE)
predval3 <- as.matrix(attr(p_val3, "probabilities"))[,'1']
c1 <- confusionMatrix(factor(p_train3), factor(Y_Model3), positive = '1')
c2 <- confusionMatrix(factor(p_val3), factor(Y_test_Model3), positive = '1')
t_err3 <- 1- c1$overall[1] #0
v_err3 <- 1 - c2$overall[1] 


#Training the Soft Margin classifier 2 (Vehicales) with Normalized feature space and determined cost:
svm_normalized_4 <- svm(normalizing_data, Y_Model4, kernel = "linear", 
                        cost=128, probability = TRUE) 
p_train4 <- predict(svm_normalized_4, normalizing_data, probability = TRUE)
p_val4 <- predict(svm_normalized_4, X_Test[,1:51949], probability = TRUE)
predval4 <- as.matrix(attr(p_val4, "probabilities"))[,'1']
c1 <- confusionMatrix(factor(p_train4), factor(Y_Model4), positive = '1')
c2 <- confusionMatrix(factor(p_val4), factor(Y_test_Model4), positive = '1')
t_err4 <- 1- c1$overall[1] #0
v_err4 <- 1 - c2$overall[1] 

#Combining the 4 classifiers
df <- cbind(predval1, predval2, predval3, predval4)
final <- max.col(df)
final <- as.factor(final)
confusionMatrix(final, Y_Test)





























