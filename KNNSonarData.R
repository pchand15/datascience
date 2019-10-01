#KNN algorithm

#install all packages
install.packages("class")
install.packages("caret")
install.packages("mlbench")
install.packages("e1071")

library(class)
library(caret)
require(mlbench)
library(e1071)
library(base)
require(base)

#Step1: Data Collection
data(Sonar)
head(Sonar)
View(Sonar)

#Step2 - Preparing and Exploring the data
cat("Num of rows and columns", nrow(Sonar), ncol(Sonar))
table(Sonar$Class)
base::table(Sonar$Class)
apply(Sonar, 2, function(x) sum(is.na(x)))

#here we want to manually take samples from data (random sampling) to split Sonar into training and test sets.
SEED = 123
set.seed(SEED)
data = Sonar[base::sample(nrow(Sonar)),] #shuffle data first
bound = floor(0.7*nrow(data))
df_train = data[1:bound,]
View(df_train)
base::table(df_train$Class)
df_test <- data[(bound+1):nrow(data),]
cat("number of training and test samples are", nrow(df_train),nrow(df_test))

#Examine the train and test samples have properly splitted with the almost the same portion of Class labels
cat("number of training classes: \n", base::table(df_train$Class)/nrow(df_train))
cat("number of test classes: \n", base::table(df_test$Class)/nrow(df_test))

#Simplify by creating the dataframe which is a subset of the actual data
X_train = subset(data[1:104,], select=-Class)
y_train = data[1:104,]$Class
X_test = subset(data[105:208,], select = -Class) #exclude the Class(dependent variable) for prediction.
y_test = data[105:208,]$Class
View(X_test)
View(X_train)

#Step 4 - Training the model on data
#Using KNN function from Class library with k=3
model_knn = knn(train=X_train, test = X_test,cl=y_train,k=3)
model_knn
dim(X_train)
dim(X_test)
dim(df_train)

#Evaluate the model performace using confusion matrix
#model_knn with k=3 provides the above predictions for the X_test. then we can see how many classes have been correctly or incorrectly classified by comparing to the true labels as follows:
conf_mat <- base::table(y_test,model_knn)
conf_mat


#accuracy
cat("Accuracy: ",sum(diag(conf_mat))/sum(conf_mat))

#To assess whether  k=3k=3  is a good choice and see whether  k=3k=3  leads to overfitting /underfitting the data, we could use knn.cv which does the leave-one-out cross-validations for training set (i.e., it singles out a training sample one at a time and tries to view it as a new example and see what class label it assigns).
#Below are the predicted classes for the training set using the leave-one-out cross-validation. Now, let's examine its accuracy

knn_loocv = knn.cv(train = X_train, cl=y_train, k=3)
knn_loocv

conf_mat_cv = base::table(y_train, knn_loocv)
conf_mat_cv
cat("LOOCV accuracy: ", sum(diag(conf_mat_cv))/sum(conf_mat_cv))

#The difference between the cross-validated accuracy and the test accuracy shows that,  k=3k=3  leads to overfitting. Perhaps we should change  kk  to lessen the overfitting.


#Step-5 Improve the performance of the model
# As noted earlier, we have not standardized (as part of preprocessing) our training and test sets. In the rest of the tutorial, we will see the effect of choosing a suitable  kk  through repeated cross-validations using caret library.
# 
# In a cross-validation procedure:
#   
#   The data is divided into the finite number of mutually exclusive subsets
# Through each iteration, a subset is set aside, and the remaining subsets are used as the training set
# The subset that was set aside is used as the test set (prediction)
# This is a method of cross-referencing the model built using its own data.

SEED = 2016
set.seed(SEED)
#create the training data 70% of the overall Sonar Data
to_train=createDataPartition(Sonar$Class, p=0.7,list=FALSE) #creating training indices
ndf_train = Sonar[to_train,]
ndf_test = Sonar[-to_train,]

#Here, we specify the cross-validation method we want to use to find the best  kk  in grid search. Later, we use the built-in plot function to assess the changes in accuracy for different choices of  k.k.
# lets create a function setup to do 5-fold cross-validation with 2 repeat.
ctrl = trainControl(method = "repeatedcv",number=5, repeats = 2)
nn_grid = expand.grid(k=c(1,3,5,7))
nn_grid

set.seed(SEED)

best_knn = train(Class~.,data=ndf_train, method="knn",trControl=ctrl,preProcess=c("center","scale"),tuneGrid=nn_grid)
best_knn
par(mar = c(12,12,12,12))
accr = best_knn$results[2]
accr
kval = best_knn$results[1]
df1 = data.frame(kval, accr)
plot(df1$kval, df1$accr,ylab="Accuracy")



















