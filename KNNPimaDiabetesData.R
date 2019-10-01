library(class)
library(caret)
require(mlbench)
library(e1071)
library(base)
require(base)

listofdata <- data(package = "mlbench")
listofdata$results #take PimaIndiansDiabetes to classify the test data whether a person has diabetes or not.

#Step 1: Data Collection
data(PimaIndiansDiabetes)
head(PimaIndiansDiabetes, 25)
dim(PimaIndiansDiabetes)
View(PimaIndiansDiabetes)

#Step 2: Preparing and Exploring the data
summary(PimaIndiansDiabetes) #check the descriptive stat and 'NAs' if any
table(PimaIndiansDiabetes$diabetes) #neg pos 500 268

#The CrossTable gives a result stating stating that people in their 50s have more proportion of diabetes.
library(gmodels)
CrossTable(PimaIndiansDiabetes$age, PimaIndiansDiabetes$diabetes, prop.r = TRUE , prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#Check NAs if any, remove 
cat("Num of rows and cols: ",
    nrow(PimaIndiansDiabetes),
    ncol(PimaIndiansDiabetes))
apply(PimaIndiansDiabetes, 2, function(x)
  sum(is.na(x)))
sum(is.na(PimaIndiansDiabetes[,9])) #no NA can check using summary too.

#Split the data into training and test set using random sampling
SEED = 123 #set a row number
set.seed(SEED)#set a random assignment point
shuffdata <- PimaIndiansDiabetes[sample(nrow(PimaIndiansDiabetes)), ]
X_train <- subset(shuffdata[1:(nrow(PimaIndiansDiabetes)/2), ], select = -diabetes)
X_test <- subset(shuffdata[(nrow(PimaIndiansDiabetes)/2 + 1):768, ], select = -diabetes)
y_train <- PimaIndiansDiabetes$diabetes[1:(nrow(PimaIndiansDiabetes)/2)]
y_test <- PimaIndiansDiabetes$diabetes[(nrow(PimaIndiansDiabetes)/2 + 1):768]

View(X_train)
View(X_test)
View(y_train)
View(y_test)

#Training the model on data
mod_knn <-
  knn(train = X_train, test = X_test, cl = y_train, k=9)
mod_knn
summary(mod_knn)

#Step 4 - Evaluate the model performance¶
#As you can see, model_knn with  k=3k=3  provides the above predictions for the test set X_test. 
#Then, we can see how many classes have been correctly or incorrectly classified by comparing to the true labels as follows
#Use confusion matrix to evaluate the value of k and hence the KNN model

conf_mat_diab <- base::table(y_test, mod_knn)
conf_mat_diab
#confusion matrix results
#mod_knn

cat("Model acccuracy on test set based on conf mat for diab: ",sum(diag(conf_mat_diab))/sum(conf_mat_diab) )
#Precision and recall 
#y_test neg pos
#neg 200  61
#pos 101  22
#Precision and recall of the moel on test set
precision = 22/sum(conf_mat_diab[1,2]+conf_mat_diab[2,2])
precision
recall= 22/(101+22)
recall

#k-fold cross validation
#To assess whether  k=3 is a good choice and see whether  k=3 leads to overfitting /underfitting the data, 
#we could use knn.cv which does the leave-one-out cross-validations for training set (i.e., it singles out a training sample one 
#at a time and tries to view it as a new example and see what class label it assigns).
knn_loo_cv <- knn.cv(train=X_train, cl=y_train, k=9)
knn_loo_cv

#Lets create a confusion matrix to compute the accuracy of the training labels y_train and the cross-validated predictions knn_loocv
#, same as the above. What can you find from comparing the LOOCV accuracy and the test accuracy above?
conf_mat_diab_cv <- base::table(y_train, knn_loo_cv)
conf_mat_diab_cv
cat("Model acccuracy after running Leave-one-out Cross validation on conf mat for diab: ", sum(diag(conf_mat_diab_cv))/sum(conf_mat_diab_cv))

#The difference between the cross-validated accuracy and the test accuracy shows that,k=9 leads to overfitting. 
#Perhaps we should change k to lessen the overfitting.
SEED = 2016
set.seed(SEED)
#create the training data from the Pima data
#In a cross-validation procedure:
#Through each iteration, a subset is set aside, and the remaining subsets are used as the training set
#The subset that was set aside is used as the test set (prediction)
#This is a method of cross-referencing the model built using its own data.
ind_train <- createDataPartition(PimaIndiansDiabetes$diabetes, p=0.7,list=FALSE)
df_train <- PimaIndiansDiabetes[ind_train,]
df_test <- PimaIndiansDiabetes[-ind_train,]
cat("Number of rows and columns in df_train: ", nrow(df_train), ncol(df_train))
cat("Number of rows and columns in df_test: ", nrow(df_test), ncol(df_test))

#Here, we specify the cross-validation method we want to use to find the best  kk  in grid search. 
#Later, we use the built-in plot function to assess the changes in accuracy for different choices of  k.

#function setup to do 5-fold cross-validation with 2 repeat.
ctrl <- trainControl(method="repeatedcv",number=3, repeats = 2)

set.seed(SEED)
best_knn <- train(diabetes~. , data=df_train, method="knn", trControl=ctrl, preProcess=c("center", "scale"))
best_knn

conf_mat_k1 <- table(y_test_k1, mod_knn_k1)
#conf mat
cat("Conf mat for k=1 ", sum(diag(conf_mat_k1))/sum(conf_mat_k1))

install.packages("scatterplot3d")
library("scatterplot3d")

#Principal Component Analysis in R
#In R, there are a few different functions for analyzing Principal Components -- the preferred and most utilized 
#one is prcomp. prcomp takes the dataset you want to analyze as a parameter with two optional parameters 
#(center and scale.), which do the work of centering (shifting your data points to be zero-centered) and scaling 
#(so your observations have unit variance) your data, which is recommended for PCA.

data(PimaIndiansDiabetes)
dim(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes$diabetes)

#Remove the non-numeric column 'diabetes'. 
diabdata_minus_DiabCol <- PimaIndiansDiabetes[,-9]

par(mar=c(15,15,15,15))
scatterplot3d(diabdata_minus_DiabCol[,2:4], main="3D Scatter Plot", xlab="Glucose", ylab="Pressure", zlab="triceps")

colnames(diabdata_minus_DiabCol)

# Perform PCA on the numerical data from "PimaIndiansDiabetes"
# prcomp does all the work of centering and scaling our data for us!
pcaSolution <- prcomp(diabdata_minus_DiabCol, center=TRUE, scale.=TRUE)

# Print a summary of the analysis
print(pcaSolution)
summary(pcaSolution)
#However, there is another way of choosing what components are going to be used -- Scree plots and the Kaiser-Guttman rule. Scree plots are 
#simple -- you just plot the eigenvalues for each component. It should be visible where there is a dropoff, which should be the cutoff for dimensions.
plot(pcaSolution, type="l", main="Eigenvalues for each Principal Component")

#The Kaiser-Guttman rule states that one should pick the dimensions based on the average eigenvalue -- if a component presents a 
#higher-than-average eigenvalue, then it should be picked. We can add this line to the Scree plot to visualize this cutoff better. 
#Eigenvalues can be calculated as the square value of the standard deviation for a principal component, like this:

#Note: Considering that your data has been centered and scaled, the mean of the eigenvalues should be equal to one.

eigenvalues <- (pcaSolution$sdev)^2
eigenvalues
mean(eigenvalues)

#Plot the scree plot
screePlot = plot(pcaSolution, type="1", main="Eigenvalues with Kaiser-Guttman cutoff")

# Add a cutoff line based on the mean of the eigenvalues.
# This should be equal to one for centered and scaled data.
abline(h=mean(eigenvalues), lty=2, col="red")

#In this case it appears that picking the first component, and possibly the second component seems like the correct choice. 
#To get the actual data with the rotation applied (aligned to the component axes), you should retrieve the x column from our 
#solution:

# Retrieve the values of the observations for each principal component.
rotated_values <- pcaSolution$x

#Print out the first few rows of data
head(rotated_values)

dim(rotated_values)
rotated_values <- as.data.frame(rotated_values)
diabetes <- PimaIndiansDiabetes[,9]
levels(diabetes) = c(1,2)
plot(
  rotated_values$PC1,
  rotated_values$PC2,
  xlab = "Principal Component 1",
  ylab = "Principal Component 2",
  pch = 20, 
  cex=2,
  col=adjustcolor(diabetes,alpha.f=0.5)
)
legend(2,2, legend=levels(PimaIndiansDiabetes[,5]),pch=20,cex=0.8,col=levels(diabetes))








































































