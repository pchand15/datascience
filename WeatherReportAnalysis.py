"""Description:This program deals with ice cream sales prediction using OLS regression model"""
# __author__ = "Pallavi Chandra"
# Project Name = "weather report analysis Using Regression model"
import os
dir="etc"
path = os.getcwd()
chmod = 0o777
try:
    if(not os.path.exists(dir)):
        os.mkdir(dir, chmod)
    else:
        print(dir + " already exists")
except OSError:
    print("Creation of directory failed.")
finally:
    print("Removing all the object as trycatch block is released")

#Perform weather report analysis using Data Analysis Python Packages
#Predict the maximum temparature taking minimum temperature as regressor variable.

#import all the required libraries
import sys
import pandas as pd #reading, creating and manipulating dataframes
import numpy as np #linear algebra
import matplotlib.pyplot as plt
#Use seaborn for DV apart from the matplotlib
import seaborn as seabornInstance
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn import metrics
from sklearn.linear_model import LogisticRegression

#dataframe
df = pd.read_csv('etc\Weather.csv')
print (df.shape)
print (df.describe())
print(df.head(10)) #View the dataframe


#Plot the data points on a 2-D graph to eyeball our dataset
"""df.plot(x='MinTemp', y='MaxTemp', style='o')
plt.title('MinTemp vs MaxTemp')
plt.xlabel('MinTemp')
plt.ylabel('MaxTemp')
plt.show()"""

#Check the avg. max temperature and once we plot it we can observe that the avg. max temperature is between 25 and 35.
plt.figure(figsize=(15,10))
plt.tight_layout()
seabornInstance.distplot(df['MaxTemp'])

#Divide the data into predictor and response variables also called attributes and labels.
#Attributes are independent variables while labels are dependent variable whose value is to be predicted.
X=df['MinTemp'].values.reshape(-1,1)
y=df['MaxTemp'].values.reshape(-1,1)

X_train, X_test, y_train, y_test=train_test_split(X,y,test_size=0.2,random_state=0)

#Now train our dataset
mod = LinearRegression()
mod.fit(X_train,y_train)#training the data

#retrieve the statistical summary
print(mod.coef_)
print(mod.intercept_)

import statsmodels.api as sm
X_train=sm.add_constant(X_train)
stmod = sm.OLS(y_train,X_train).fit()
print(stmod.summary())















