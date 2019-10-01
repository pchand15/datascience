#one time activity to copy the .csv files inside the "etc" folder.
if(!file.exists("etc")){
  dir.create("etc")
}
#Replace with file.choose() if do not wish to keep the files in etc folder.
trainingdf <- read.csv("etc\\application_train.csv", header = T, sep=",")
sampledf <- read.csv("etc\\sample_submission.csv", header = T, sep=",")
testdf <- read.csv("etc\\application_test.csv", header = T, sep = ",")

#Understanding DataSet
######################
head(trainingdf, 10)
colnames(trainingdf)
str(trainingdf)

#Understanding the data structure of variables(unexpected trend, anamolies)
##############################################
library(gmodels)
CrossTable(trainingdf$NAME_HOUSING_TYPE)

#Check the relation between target(default loans - repsonse variable) and housing type(predictor variable). 
CrossTable(trainingdf$NAME_HOUSING_TYPE, trainingdf$TARGET, prop.r = TRUE,prop.c = FALSE ,prop.t = FALSE, prop.chisq = FALSE)


#Removing Outliers and Data Visualization - perform on training dataset
#########################################

#Make a copy of the data
trainingdfcp <- trainingdf

#Scatter plot of income of the applicants
#Set figure margin to fit
par(mar = c(3,4,1,4))
plot(trainingdf$AMT_INCOME_TOTAL, ylab = "Total Annual Income")

#Histogram for the interest rate - calculated from the annuity
par(mar=c(4,4,4,4))
histannuity <- hist(trainingdf$AMT_ANNUITY, main= "Annuity amount", xlab = "Annuity paid")
histannuity$breaks

#Histogram of the total annual income data
histinc <- hist(trainingdf$AMT_INCOME_TOTAL, main="Total Annual Income", xlab = "Total Annual income") #for total annual income.
options(scipen=999)
histinc$breaks

#Histogram of the credit loan amount
hist_credit <- hist(trainingdfcp$AMT_CREDIT, xlab = "Loan amount Credited") #for credited loan amount
plot(trainingdfcp$AMT_CREDIT, ylab = "Credited Loan amount")
head(trainingdfcp$DAYS_BIRTH,5) #understanding the DAYS_BIRTH values 

#Convert the DAYS_BIRTH into age and store in a new column named age
todays_date = Sys.Date() # For now take the system date. The ideal way of calculating will be from the date of registration DAYS_REGISTRATION if the data has not been updated and all the fields have been populated on the day of registration.  
library(eeptools)
#The system generated date time format is %Y-%m-%d. Use the same format to perform subtration operation
trainingdfcp$DAYS_BIRTH = abs(trainingdfcp$DAYS_BIRTH) #take the absolute value
trainingdfcp$dob = as.Date(as.character(todays_date), format = "%Y-%m-%d") - trainingdfcp$DAYS_BIRTH #find the date of birth from the given days_birth w.r.t current date.
head(trainingdfcp$dob,20) #check if the dob is in current date character format

trainingdfcp$age = age_calc(trainingdfcp$dob, enddate = todays_date, units = "years",precise = TRUE)
head(trainingdfcp$age,20) #check if the calculated age is in floating point as precise is given true
#trainingdfcp$age = round(as.numeric(trainingdfcp$age), digits = 0) #Uncomment if running age as discrete variable to run the binomial model.Convert age variable into the integer format. However, for fit to model consider age as continuous variable and do not include this step. 

#Histogram of the age
summary(trainingdfcp$age)
hist(trainingdfcp$age, xlab = "Age of the applicant")
par(mar=c(4,4,4,4))
plot(trainingdfcp$age, ylab="Age of the applicant")
plot(trainingdfcp$age, trainingdfcp$AMT_INCOME_TOTAL, xlab="Age", ylab = "Total Income")

#Remove Total income outliers before fitting the binomial model
summary(trainingdfcp$AMT_INCOME_TOTAL) #the 75th quantile is at 202500.
inc_cutoff <- quantile(trainingdfcp$AMT_INCOME_TOTAL, 0.75) + (1.5*IQR(trainingdfcp$AMT_INCOME_TOTAL)) #337500 
ind_inc_cutoff <- which(trainingdfcp$AMT_INCOME_TOTAL > inc_cutoff) #10740 
traindf_after_rmcutoff <- trainingdfcp[-ind_inc_cutoff,]

#Check the dimensions after removing the outlier indeces
dim(trainingdfcp) #257512, 122
dim(traindf_after_rmcutoff) #245772, 122 --- 11740 obs. affected. Let us keep it as it is as outliers will cause model to deviate as SS will increase.  


#let us see the histogram after outlier removal
hist(traindf_after_rmcutoff$AMT_INCOME_TOTAL, breaks = sqrt(nrow(traindf_after_rmcutoff)),xlab = "Total Annual income without Outliers")
hist(traindf_after_rmcutoff$AMT_CREDIT, xlab = "Credited Loan amount")
plot(traindf_after_rmcutoff$AMT_CREDIT, xlab = "Credited Loan amount")

#Summarize the training dataset AMT_CREDIT continuous variable to get the quartiles.
summary(trainingdfcp$AMT_CREDIT)
hist_credit$breaks

#Handling Missing Data 
summary(trainingdfcp$AMT_ANNUITY)
View(trainingdfcp)
hist_annuity <- hist(trainingdfcp$AMT_ANNUITY, xlab="Annuity value")
hist_annuity$breaks
plot(trainingdfcp$AMT_ANNUITY, ylab = "Annuity")

#bin the values based on summary results
summary(traindf_after_rmcutoff$AMT_ANNUITY)
quantile(trainingdfcp$AMT_ANNUITY, na.rm=TRUE)

#make the bins based on quartiles
trainingdfcp$CAT_ANNUITY <- rep(NA, length(trainingdfcp$AMT_ANNUITY))
trainingdfcp$CAT_ANNUITY[which(trainingdfcp$AMT_ANNUITY <= 1616)] <- "0-1616"
trainingdfcp$CAT_ANNUITY[which(trainingdfcp$AMT_ANNUITY > 1616 & trainingdfcp$AMT_ANNUITY <= 16542)] <- "1616-16542"
trainingdfcp$CAT_ANNUITY[which(trainingdfcp$AMT_ANNUITY > 16542 & trainingdfcp$AMT_ANNUITY <= 24903)] <- "16542-24903"
trainingdfcp$CAT_ANNUITY[which(trainingdfcp$AMT_ANNUITY > 24903 & trainingdfcp$AMT_ANNUITY <= 34596)] <- "24903-34596"
trainingdfcp$CAT_ANNUITY[which(trainingdfcp$AMT_ANNUITY > 34596 & trainingdfcp$AMT_ANNUITY <= 230162)] <- "34596-230162"
trainingdfcp$CAT_ANNUITY[which(is.na(trainingdfcp$AMT_ANNUITY))] <- "Missing"
head(trainingdfcp$CAT_ANNUITY, 40)
#Convert the CAT_ANNUITY into factor variable
trainingdfcp$CAT_ANNUITY <- as.factor(trainingdfcp$CAT_ANNUITY)

#plot the new factor variable CAT_ANNUITY
plot(trainingdfcp$CAT_ANNUITY)



#Dimesions of the test set and training set to minimize the sample variation, if any
dim(testdf) #49999
dim(trainingdfcp) #257512

tot_obs <- 257512+49999 
#Test if the data proportion is 2/3 and 1/3
(2/3)*tot_obs #205007.3 which is higher than the trainingdfcp observations. We are good to go.


#Fit the model
bin_mod <- glm(TARGET ~ age + AMT_CREDIT + NAME_HOUSING_TYPE + AMT_INCOME_TOTAL, data=trainingdfcp, family=binomial)
summary(bin_mod)

#update the model with DAYS_EMPLOYED to get the length an applicant is employed.
trainingdfcp$CAT_EMP_LEN <- trainingdfcp$DAYS_EMPLOYED/365
trainingdfcp$CAT_EMP_LEN <- abs(trainingdfcp$CAT_EMP_LEN)
trainingdfcp$CAT_EMP_LEN <- round(trainingdfcp$CAT_EMP_LEN)
quantile(trainingdfcp$CAT_EMP_LEN)
head(trainingdfcp$DAYS_EMPLOYED, 10)
plot(trainingdfcp$CAT_EMP_LEN, ylab="Applicant Employment-leng")

quantile(trainingdfcp$CAT_EMP_LEN)

#form employment bins based on quantiles. DO NOT REMOVE THE OUTLIERS.
trainingdfcp$FACT_EMP_LEN <- rep(NA, length(trainingdfcp$CAT_EMP_LEN))
trainingdfcp$FACT_EMP_LEN[which(trainingdfcp$CAT_EMP_LEN <= 3)] = "0-3"
trainingdfcp$FACT_EMP_LEN[which(trainingdfcp$CAT_EMP_LEN > 3 & trainingdfcp$CAT_EMP_LEN <= 6)] = "3-6"
trainingdfcp$FACT_EMP_LEN[which(trainingdfcp$CAT_EMP_LEN > 6 & trainingdfcp$CAT_EMP_LEN <= 16)] = "6-16"
trainingdfcp$FACT_EMP_LEN[which(trainingdfcp$CAT_EMP_LEN > 16)] = "16-1001"
trainingdfcp$FACT_EMP_LEN <- as.factor(trainingdfcp$FACT_EMP_LEN)
str(trainingdfcp$FACT_EMP_LEN)
head(trainingdfcp$FACT_EMP_LEN,10)


bin_mod2<-update(bin_mod, ~.+FACT_EMP_LEN)
summary(bin_mod2)

#observed vs predicted data frame
loan_status <-
  data.frame(
    SK_ID_CURR = trainingdfcp$SK_ID_CURR,
    TARGET = trainingdfcp$TARGET,
    TARGET_PRED = predict(bin_mod2, type = "response")
  )
loan_status$TARGET_PRED <-
  round(loan_status$TARGET_PRED, digits = 1)
head(loan_status, 50)


#create a column for factored emp length variable to test on the binomial fit-model
testdf$CAT_EMP_LEN <- testdf$DAYS_EMPLOYED/365
testdf$CAT_EMP_LEN <- abs(testdf$CAT_EMP_LEN)
testdf$CAT_EMP_LEN <- round(testdf$CAT_EMP_LEN)
quantile(testdf$CAT_EMP_LEN)
head(testdf$DAYS_EMPLOYED, 10)
plot(testdf$CAT_EMP_LEN, ylab="Applicant Employment-leng")

quantile(testdf$CAT_EMP_LEN)

#form employment bins based on quantiles. DO NOT REMOVE THE OUTLIERS.
testdf$FACT_EMP_LEN <- rep(NA, length(testdf$CAT_EMP_LEN))
testdf$FACT_EMP_LEN[which(testdf$CAT_EMP_LEN <= 3)] = "0-3"
testdf$FACT_EMP_LEN[which(testdf$CAT_EMP_LEN > 3 & testdf$CAT_EMP_LEN <= 6)] = "3-6"
testdf$FACT_EMP_LEN[which(testdf$CAT_EMP_LEN > 6 & testdf$CAT_EMP_LEN <= 16)] = "6-16"
testdf$FACT_EMP_LEN[which(testdf$CAT_EMP_LEN > 16)] = "16-1001"
testdf$FACT_EMP_LEN <- as.factor(testdf$FACT_EMP_LEN)

nrow(testdf)
dim(trainingdfcp)
dim(trainingdf)

todays_date = Sys.Date() # For now take the system date. The ideal way of calculating will be from the date of registration DAYS_REGISTRATION if the data has not been updated and all the fields have been populated on the day of registration.  
library(eeptools)
#The system generated date time format is %Y-%m-%d. Use the same format to perform subtration operation
testdf$DAYS_BIRTH = abs(testdf$DAYS_BIRTH) #take the absolute value
testdf$dob = as.Date(as.character(todays_date), format = "%Y-%m-%d") - testdf$DAYS_BIRTH #find the date of birth from the given days_birth w.r.t current date.
head(testdf$dob,20) #check if the dob is in current date character format

testdf$age = age_calc(testdf$dob, enddate = todays_date, units = "years",precise = TRUE)
head(testdf$age,20) #check if the calculated age is in floating point as precise is given true
#trainingdfcp$age = round(as.numeric(trainingdfcp$age), digits = 0) #Uncomment if running age as discrete variable to run the binomial model.Convert age variable into the integer format. However, for fit to model consider age as continuous variable and do not include this step. 

#Sample test set from testdf containing explanatory variables for bin_mod2 

#Predict the loan default on test data using the bin_mod 2
tcPredVal <- predict(bin_mod2, newdata = testdf, type="response")

#Look at the range of predictions to define cut-off values
range(tcPredVal) #0.0000000000000002220446 0.1856655715119401861557

######################################################################################################################
# add the education and see the interaction terms(continuous vars) - regression function is non-linear
# Hypothesis - effect on income with one year of added work experience that will depend on number of years of education
# extract the no. of years of education based on categorial var: NAME_EDUCATION_TYPE
######################################################################################################################

#Check the factor levels
trainingdfcp$NUM_EDUCATION_YRS = rep(NA, length(trainingdfcp$NAME_EDUCATION_TYPE))
levels(trainingdf$NAME_EDUCATION_TYPE)#"Academic degree","Higher education","Incomplete higher","Lower secondary","Secondary / secondary special"
trainingdfcp$NUM_EDUCATION_YRS[which(trainingdfcp$NAME_EDUCATION_TYPE == "Academic degree" | trainingdfcp$NAME_EDUCATION_TYPE == "Higher education")]= 15
trainingdfcp$NUM_EDUCATION_YRS[which(trainingdfcp$NAME_EDUCATION_TYPE == "Incomplete higher")] = 13.5
trainingdfcp$NUM_EDUCATION_YRS[which(trainingdfcp$NAME_EDUCATION_TYPE == "Lower secondary")] = 10
trainingdfcp$NUM_EDUCATION_YRS[which(trainingdfcp$NAME_EDUCATION_TYPE == "Secondary / secondary special") ] = 12

head(trainingdfcp$NUM_EDUCATION_YRS, 10)
options(scipen = 999)
#Effect on income - interaction terms - non-linear regression model 
#library(cplm)
nlm <- lm(AMT_INCOME_TOTAL ~ NUM_EDUCATION_YRS + CAT_EMP_LEN + NUM_EDUCATION_YRS:CAT_EMP_LEN,data=trainingdfcp)
#just try for 50 data points
summary(nlm)


#test model
bin_mod3<-update(bin_mod2, ~.+NUM_EDUCATION_YRS+NUM_EDUCATION_YRS:CAT_EMP_LEN)
summary(bin_mod3)

testdf$NUM_EDUCATION_YRS = rep(NA, length(testdf$NAME_EDUCATION_TYPE))
levels(testdf$NAME_EDUCATION_TYPE)#"Academic degree","Higher education","Incomplete higher","Lower secondary","Secondary / secondary special"
testdf$NUM_EDUCATION_YRS[which(testdf$NAME_EDUCATION_TYPE == "Academic degree" | testdf$NAME_EDUCATION_TYPE == "Higher education")]= 15
testdf$NUM_EDUCATION_YRS[which(testdf$NAME_EDUCATION_TYPE == "Incomplete higher")] = 13.5
testdf$NUM_EDUCATION_YRS[which(testdf$NAME_EDUCATION_TYPE == "Lower secondary")] = 10
testdf$NUM_EDUCATION_YRS[which(testdf$NAME_EDUCATION_TYPE == "Secondary / secondary special") ] = 12
tcPred_mod3 <- predict(bin_mod3, newdata = testdf, type="response")
summary(tcPred_mod3_pct)
range(tcPred_mod3)
tcPred_mod3_pct = tcPred_mod3*100

tcPred_pct_21 <- ifelse(tcPred_mod3_pct >= 21, 1, 0)
tcdf_cutoff <- data.frame(SK_ID_CURR = testdf$SK_ID_CURR, TARGET = tcPred_pct_21)
#tcdf_cutoff <- data.frame(SK_ID_CURR = testdf$SK_ID_CURR, TARGET = tcPredVal, TARGET_PCT = tcPred_pct, TARGET_CUTOFF_15 = tcPred_pct_15, TARGET_CUTOFF_11 = tcPred_pct_11)
write.csv(tcdf_cutoff, "testcasePredict_intterms_20.csv")
#Evaluating a model
# str(traindf_after_rmcutoff$CAT_EMP_LEN)
# summary(traindf_after_rmcutoff$CAT_EMP_LEN) #the 75th quantile is at 16
# ind_empln_cutoff <- which(traindf_after_rmcutoff$CAT_EMP_LEN == 1001) #10740 
# traindf_after_rmcutoff <- traindf_after_rmcutoff[-ind_inc_cutoff,]

#Since Binomial model uses Fisher iterations, calculate the root mean square error to see the difference between the obs. vs. pred values
#Cannot run on test set as the test set does not contain corresponding obs values i.e. response variable TARGET

ld_err <- abs(loan_status$TARGET_PRED - loan_status$TARGET)
mod_rmse <- sqrt(mean(ld_err^2))
mod_rmse #root-Mean Square Error of the predicted vs. observed on the training model. for bin_mod = 0.2714391, for bin_mod2 = 0.2710168

#Second model can be taken for inference(generalization), out-of-the-sample.Choosing the second model(bin_mod2), evaluate this model and its fit.
#Deciding the cutoff value. 
#find the median of the range of values for predicted loan defaults over test set
summary(tcPredVal) #Median = 0.07599, Mean = 0.08077, Min = 0, Max = 0.18567 i.e. the model provides 0% default chance to 18.5% of default chance
#Convert into percentage 
tcPred_pct <- tcPredVal*100
head(tcPred_pct)
summary(tcPred_pct)
hist(tcPred_pct, main = "Predicted Loan Default", xlab = "Prob. of loan default(%)", breaks = 19) #Try plotting the frequency of numbers to test the accuracy and senstivity of the bin_mod2.

# Evaluation 1 test data: Based on summary, Cutoff at 11% and check the accuracy and senstivity of the model
ldpred_cutoff_11 <- ifelse(tcPred_pct >= 11, 1, 0)

#Evaluation 2 training data: 
summary(loan_status$TARGET_PRED)
ld_pred_pct <- loan_status$TARGET_PRED*100
head(ld_pred_pct)
summary(ld_pred_pct)
hist(ld_pred_pct, main = "Predicted Loan Default", xlab = "Prob. of loan default(%)", breaks = 19)

#Set Cutoff >= 15% for measuring accuracy, senstivity
ldr_cutoff_15 <- ifelse(ld_pred_pct >= 15, 1, 0)

#Confusion matrix with cutoff value 15%
cfmat_15 <- table(loan_status$TARGET, ldr_cutoff_15)
cfmat_15

#Set Cutoff >= 11% for measuring accuracy, senstivity
ldr_cutoff_11 <- ifelse(ld_pred_pct >= 11, 1, 0)

#Confusion matrix with cutoff value 15%
cfmat_11 <- table(loan_status$TARGET, ldr_cutoff_11)
cfmat_11

#test data set predicted value df
tcdf <- data.frame(SK_ID_CURR = testdf$SK_ID_CURR, TARGET = tcPredVal)
write.csv(tcdf, "testsetPred.csv")
tcPred_pct_18.5 <- ifelse(tcPred_pct >= 18.567, 1, 0)
tcPred_pct_17 <- ifelse(tcPred_pct >= 17, 1, 0)
tcdf_cutoff <- data.frame(SK_ID_CURR = testdf$SK_ID_CURR, TARGET = tcPred_pct_18.5)
#tcdf_cutoff <- data.frame(SK_ID_CURR = testdf$SK_ID_CURR, TARGET = tcPredVal, TARGET_PCT = tcPred_pct, TARGET_CUTOFF_15 = tcPred_pct_15, TARGET_CUTOFF_11 = tcPred_pct_11)
write.csv(tcdf_cutoff, "testcasePredict_80.csv")
#Create a function later to calculate ASS, Comapring the cutoffs by calculating accuracy, senstivity and specificity to define the prob model.
tot_matval_11 <- cfmat_11[1,1] + cfmat_11[1,2] + cfmat_11[2,1] + cfmat_11[2,2]
tot_matval_15 <- cfmat_15[1,1] + cfmat_15[1,2] + cfmat_15[2,1] + cfmat_15[2,2]


#Comapring accuracy at 11% default rate and 15% default rate
cfmat_11_acc = (cfmat_11[1,1]+cfmat_11[2,2])/tot_matval_11 #Accuracy of the model: 78.57%
cfmat_15_acc = (cfmat_15[1,1]+cfmat_15[2,2])/tot_matval_15 #Accuracy of the model with cutoff 15%: 91.118%

#Senstivity
cfmat_11_senst <- cfmat_11[2,2]/(cfmat_11[2,1]+cfmat_11[2,2]) #Senstivity: 27.98
cfmat_11_senst
cfmat_15_senst <- cfmat_15[2,2]/(cfmat_15[2,1]+cfmat_15[2,2]) #Senstivity: 1.58
cfmat_15_senst

#Specificity. No default customers categorized correctly
cfmat_11_spec <- cfmat_11[1,1]/(cfmat_11[1,1]+cfmat_11[1,2]) #Specificity: 83.01
cfmat_15_spec <- cfmat_15[1,1]/(cfmat_15[1,1]+cfmat_15[1,2]) #Specificity: 99.05%


###################################################################
#Decision Tree/Regression tree. Creating the decision tree using 
#loss matrix to remove the issue of accuracy.                                 
#                                                                
###################################################################
library(rpart)
set.seed(345)
regtree_loss_matrix <-
  rpart(
    TARGET ~ age + AMT_CREDIT + NAME_HOUSING_TYPE + AMT_INCOME_TOTAL + FACT_EMP_LEN,
    method = "class",
    data = trainingdfcp,
    control = rpart.control(cp = 0.001),
    parms = list(loss=matrix(c(0,10,1,0), ncol=2))
  )
#Graphical representation of the decision tree
plot(regtree_loss_matrix, uniform = TRUE)

#split of the tree
text(regtree_loss_matrix, digits = 4, use.n = TRUE)

#Plot the cross-validated error rate as a function of cp
plotcp(regtree_loss_matrix)

#Check for which cp value xerror is minimum
printcp(regtree_loss_matrix) #cp = 0.0813164

#Create an index for all the row with minimum xerror
index <- which.min(regtree_loss_matrix$cptable[,"xerror"])
min_cp <- regtree_loss_matrix$cptable[index, "CP"]

#Prune the tree created using loss matrix and use minimum error rate and corresponding cp value.Hold the cp at 0.001 for now.
pruned_regtree_loss_mat <- prune(regtree_loss_matrix,cp=0.001)

#plot the pruned regression tree using loss matrix to penalize the misclassified defaults and non-defaults.
library(rpart.plot)
prp(pruned_regtree_loss_mat, extra = 1) #See the labels to see the node value cut-offs

#Evaluating the constructed model
#################################

#First decide the accept rate for loan approvals and bad rate for predicting loan defaults using pruned tree model(Since accuracy issue is nullified.)

#Draw sample inference to population by making predictions for the probability of default using the pruned tree model on test set data
prob_default_lossmat <- predict(pruned_regtree_loss_mat, newdata = trainingdfcp)[ ,2] #Default value stored in 2nd column

#accetance rate cutoff at 0.8
loan_accept_cutoff <- quantile(prob_default_lossmat, 0.8)

#Condition to get binary form of credit loan status
bin_pred_accept_80 <- ifelse(prob_default_lossmat > loan_accept_cutoff, 1, 0)

#Actual default status of accepted loans
actual_accepted_loans <- trainingdfcp$TARGET[bin_pred_accept_80 == 0]

#Calculate bad rate of the accepted loans
bad_loan_ind <- which(actual_accepted_loans == 1)

#Percentage of bad rate loans
bad_rate <- (length(bad_loan_ind)/length(actual_accepted_loans))*100
bad_rate #% of defaults for the accepted loans/bad rate: 7.054195%

#Strategy Curve and Strategy Table
pred_binmod <- predict(bin_mod2, newdata = trainingdfcp, type="response")
strategy_tab_lossmat <- strategy_table(prob_default_lossmat)
strategy_tab_binmod <- strategy_table(pred_binmod)
strategy_tab_binmod

#Strategy Curve and Strategy Table for bin_mod3
pred_binmod3 <- predict(bin_mod3, newdata = trainingdfcp, type="response")
strategy_tab_lossmat3 <- strategy_table(prob_default_lossmat)
strategy_tab_binmod3 <- strategy_table(pred_binmod3)
strategy_tab_binmod3 

#Plot the curve for binmod predictions based strategy table
par(mfrow = c(1,2))
plot(strategy_tab_binmod$accept_rate, strategy_tab_binmod$bad_rate, 
     type = "l", xlab = "Acceptance rate", ylab = "Bad rate", 
     lwd = 2, main = "logistic regression")

#Plot the strategy curve for predictions using loss matrix classification tree
par(mar = c(3, 3, 3, 3))
plot(strategy_tab_lossmat$accept_rate, strategy_tab_lossmat$bad_rate, 
     type = "l", xlab = "Acceptance rate", ylab = "Bad rate", 
     lwd = 2, main = "Classification Tree - loss matrix")


#Area under ROC curve
library(pROC)
#ROC curve for bin_mod2
ROC_binmod <- roc(trainingdfcp$TARGET, pred_binmod)
ROC_binmod3 <- roc(trainingdfcp$TARGET, pred_binmod3)
ROC_binmod4 <- roc(trainingdfcp$TARGET, pred_binmod4)

#ROC curve for classification tree
ROC_tree <- roc(trainingdfcp$TARGET, prob_default_lossmat)

# Draw all ROCs on one plot
plot(ROC_binmod4)
lines(ROC_binmod3, col="blue")
lines(ROC_binmod, col="green")
lines(ROC_tree, col="red")
lines(ROC_binmod4, col="orange")

# Compute the AUCs
auc(ROC_binmod)
auc(ROC_tree)
auc(ROC_binmod3)
auc(ROC_binmod4)


#Define a function to calculate the strategy table with different cut-off rates
strategy_table <- function(prob_default_lossmat){
  cutoff=rep(NA, 21)
  bad_rate=rep(NA, 21)
  accept_rate=seq(1,0,by=-0.05)
  for (i in 1:21){
    cutoff[i]=quantile(prob_default_lossmat,accept_rate[i])
    pred_i=ifelse(prob_default_lossmat> cutoff[i], 1, 0)
    pred_as_good=trainingdfcp$TARGET[pred_i==0]
    bad_rate[i]=sum(pred_as_good)/length(pred_as_good)}
  table=cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4))
  return(list(table=table,bad_rate=bad_rate, accept_rate=accept_rate, cutoff=cutoff))
}

#Data Visualization
strategy_tab_lossmat <- data.frame(strategy_tab_lossmat)
#Graphs and charts
library(plotly)
par(mar = c(4,4,4,4))
loan_status_pred_plot <-
  plot_ly(
    strategy_tab_lossmat,
    x =  ~ accept_rate*100,
    y =  ~ (strategy_tab_lossmat$bad_rate)*100,
    type = 'bar',
    name = 'Bad rate',
    text = (strategy_tab_lossmat$bad_rate)*100,
    textposition = 'auto'
  ) %>% layout(yaxis = list(title = "Probability of Default(%)"), barmode = 'bar') 

loan_status_pred_plot

write.csv(strategy_tab_binmod$table, "strategy_tab_binmod.csv")
write.csv(strategy_tab_lossmat$table, "strategy_tab_lossmat.csv")




##############################New Model using other possible predictor variables#############################################

cor(trainingdfcp$EXT_SOURCE_1, trainingdf$TARGET, use = "complete.obs")
length(complete.cases(trainingdfcp$EXT_SOURCE_1))
summary(trainingdfcp$EXT_SOURCE_1)


tempdf <- which(trainingdfcp$EXT_SOURCE_1 == "NA")
length(tempdf)

trainingdfcp$FACT_EXT_SRC_1 <- rep(NA, length(trainingdfcp$EXT_SOURCE_1))
trainingdfcp$FACT_EXT_SRC_1[which(trainingdfcp$EXT_SOURCE_1 <= 0.33)] <- "0-0.33"
trainingdfcp$FACT_EXT_SRC_1[which(trainingdfcp$EXT_SOURCE_1 > 0.33 & trainingdfcp$EXT_SOURCE_1 <= 0.50)] <- "0.33-0.50"
trainingdfcp$FACT_EXT_SRC_1[which(trainingdfcp$EXT_SOURCE_1 > 0.50 & trainingdfcp$EXT_SOURCE_1 <= 0.68)] <- "0.50-0.68"
trainingdfcp$FACT_EXT_SRC_1[which(trainingdfcp$EXT_SOURCE_1 > 0.68)] <- "0.68-0.96"
trainingdfcp$FACT_EXT_SRC_1[which(is.na(trainingdfcp$EXT_SOURCE_1))] <- "missing"

trainingdfcp$FACT_EXT_SRC_1 <- as.factor(trainingdfcp$FACT_EXT_SRC_1)
levels(trainingdfcp$FACT_EXT_SRC_1)
CrossTable(trainingdfcp$FACT_EXT_SRC_1, trainingdfcp$TARGET, prop.r = TRUE,prop.c = FALSE ,prop.t = FALSE, prop.chisq = FALSE)

trainingdfcp$FACT_EXT_SRC_2 <- rep(NA, length(trainingdfcp$EXT_SOURCE_2))
trainingdfcp$FACT_EXT_SRC_2[which(trainingdfcp$EXT_SOURCE_2 <= 0.3926)] <- "0-0.3926"
trainingdfcp$FACT_EXT_SRC_2[which(trainingdfcp$EXT_SOURCE_2 > 0.3926 & trainingdfcp$EXT_SOURCE_2 <= 0.5145)] <- "0.3926-0.5145"
trainingdfcp$FACT_EXT_SRC_2[which(trainingdfcp$EXT_SOURCE_2 > 0.5145 & trainingdfcp$EXT_SOURCE_2 <= 0.6637)] <- "0.5145-0.6637"
trainingdfcp$FACT_EXT_SRC_2[which(trainingdfcp$EXT_SOURCE_2 > 0.6637 & trainingdfcp$EXT_SOURCE_2 <= 0.8550)] <- "0.6637-0.8550"
trainingdfcp$FACT_EXT_SRC_2[which(is.na(trainingdfcp$EXT_SOURCE_2))] <- "missing"

trainingdfcp$FACT_EXT_SRC_2 <- as.factor(trainingdfcp$FACT_EXT_SRC_2)
levels(trainingdfcp$FACT_EXT_SRC_2)
CrossTable(trainingdfcp$FACT_EXT_SRC_2, trainingdfcp$TARGET, prop.r = TRUE,prop.c = FALSE ,prop.t = FALSE, prop.chisq = FALSE)

trainingdfcp$FACT_EXT_SRC_3 <- rep(NA, length(trainingdfcp$EXT_SOURCE_3))
trainingdfcp$FACT_EXT_SRC_3[which(trainingdfcp$EXT_SOURCE_3 <= 0.37)] <- "0-0.37"
trainingdfcp$FACT_EXT_SRC_3[which(trainingdfcp$EXT_SOURCE_3 > 0.37 & trainingdfcp$EXT_SOURCE_3 <= 0.51)] <- "0.37-0.51"
trainingdfcp$FACT_EXT_SRC_3[which(trainingdfcp$EXT_SOURCE_3 > 0.51 & trainingdfcp$EXT_SOURCE_3 <= 0.67)] <- "0.51-0.67"
trainingdfcp$FACT_EXT_SRC_3[which(trainingdfcp$EXT_SOURCE_3 > 0.67)] <- "0.67-0.89"
trainingdfcp$FACT_EXT_SRC_3[which(is.na(trainingdfcp$EXT_SOURCE_3))] <- "missing"

trainingdfcp$FACT_EXT_SRC_3 <- as.factor(trainingdfcp$FACT_EXT_SRC_3)
levels(trainingdfcp$FACT_EXT_SRC_3)
CrossTable(trainingdfcp$FACT_EXT_SRC_3, trainingdfcp$TARGET, prop.r = TRUE,prop.c = FALSE ,prop.t = FALSE, prop.chisq = FALSE)


bin_mod4<-update(bin_mod3, ~.+FACT_EXT_SRC_1 + FACT_EXT_SRC_2 + FACT_EXT_SRC_3)
summary(bin_mod4)

pred_binmod4 <- predict(bin_mod4, newdata = trainingdfcp, type="response")
summary(pred_binmod4)
####################Applying on testdf*********************************
testdf$FACT_EXT_SRC_1 <- rep(NA, length(testdf$EXT_SOURCE_1))
testdf$FACT_EXT_SRC_1[which(testdf$EXT_SOURCE_1 <= 0.33)] <- "0-0.33"
testdf$FACT_EXT_SRC_1[which(testdf$EXT_SOURCE_1 > 0.33 & testdf$EXT_SOURCE_1 <= 0.50)] <- "0.33-0.50"
testdf$FACT_EXT_SRC_1[which(testdf$EXT_SOURCE_1 > 0.50 & testdf$EXT_SOURCE_1 <= 0.68)] <- "0.50-0.68"
testdf$FACT_EXT_SRC_1[which(testdf$EXT_SOURCE_1 > 0.68)] <- "0.68-0.96"
testdf$FACT_EXT_SRC_1[which(is.na(testdf$EXT_SOURCE_1))] <- "missing"

testdf$FACT_EXT_SRC_1 <- as.factor(testdf$FACT_EXT_SRC_1)
levels(testdf$FACT_EXT_SRC_1)

testdf$FACT_EXT_SRC_2 <- rep(NA, length(testdf$EXT_SOURCE_2))
testdf$FACT_EXT_SRC_2[which(testdf$EXT_SOURCE_2 <= 0.3926)] <- "0-0.3926"
testdf$FACT_EXT_SRC_2[which(testdf$EXT_SOURCE_2 > 0.3926 & testdf$EXT_SOURCE_2 <= 0.5145)] <- "0.3926-0.5145"
testdf$FACT_EXT_SRC_2[which(testdf$EXT_SOURCE_2 > 0.5145 & testdf$EXT_SOURCE_2 <= 0.6637)] <- "0.5145-0.6637"
testdf$FACT_EXT_SRC_2[which(testdf$EXT_SOURCE_2 > 0.6637 & testdf$EXT_SOURCE_2 <= 0.8550)] <- "0.6637-0.8550"
testdf$FACT_EXT_SRC_2[which(is.na(testdf$EXT_SOURCE_2))] <- "missing"

testdf$FACT_EXT_SRC_2 <- as.factor(testdf$FACT_EXT_SRC_2)
levels(testdf$FACT_EXT_SRC_2)

testdf$FACT_EXT_SRC_3 <- rep(NA, length(testdf$EXT_SOURCE_3))
testdf$FACT_EXT_SRC_3[which(testdf$EXT_SOURCE_3 <= 0.37)] <- "0-0.37"
testdf$FACT_EXT_SRC_3[which(testdf$EXT_SOURCE_3 > 0.37 & testdf$EXT_SOURCE_3 <= 0.51)] <- "0.37-0.51"
testdf$FACT_EXT_SRC_3[which(testdf$EXT_SOURCE_3 > 0.51 & testdf$EXT_SOURCE_3 <= 0.67)] <- "0.51-0.67"
testdf$FACT_EXT_SRC_3[which(testdf$EXT_SOURCE_3 > 0.67)] <- "0.67-0.89"
testdf$FACT_EXT_SRC_3[which(is.na(testdf$EXT_SOURCE_3))] <- "missing"

testdf$FACT_EXT_SRC_3 <- as.factor(testdf$FACT_EXT_SRC_3)
levels(testdf$FACT_EXT_SRC_3)
which(is.na(tcpred_binmod4))

tcpred_binmod4 <- predict(bin_mod4, newdata = testdf, type="response")
summary(tcpred_binmod4)
range(tcpred_binmod4)
tcpred_binmod4_pct = tcpred_binmod4*100
summary(tcpred_binmod4_pct)

#Strategy Curve and Strategy Table
pred_binmod4 <- predict(bin_mod4, newdata = trainingdfcp, type="response")
strategy_tab_binmod4 <- strategy_table(pred_binmod4)
strategy_tab_binmod4
summary(pred_binmod4)
tcPred_pct_21 <- ifelse(pred_binmod4 >= 42, 1, 0)
tcdf_cutoff <- data.frame(SK_ID_CURR = testdf$SK_ID_CURR, TARGET = tcpred_binmod4)
#tcdf_cutoff <- data.frame(SK_ID_CURR = testdf$SK_ID_CURR, TARGET = tcPredVal, TARGET_PCT = tcPred_pct, TARGET_CUTOFF_15 = tcPred_pct_15, TARGET_CUTOFF_11 = tcPred_pct_11)
write.csv(tcdf_cutoff, "testcasePred_42.csv")

table(trainingdfcp$TARGET, tcPred_pct_21)
dim(trainingdfcp)

pr_vs_obs <- data.frame(trainingdfcp$TARGET, pred_binmod4)
head(pr_vs_obs,50)





