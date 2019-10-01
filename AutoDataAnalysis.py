"""Description:Let's say we have a friend named Tom.
Problem: And Tom wants to sell his car.
But the problem is, he doesn't know how much he should sell his car for.

Constraints: Tom wants to sell his car for as much as he can.
But he also wants to set the price reasonably so someone would want to purchase it.
So the price he sets should represent the value of the car.

**************************************************************
Analysis/hypothesis: How can we help Tom determine the best price for his car?
For example, is there data on the prices of other cars and their characteristics?
What features of cars affect their prices?
Colour?
Brand?
Does horsepower also affect the selling price, or perhaps, something else?
As a data analyst or data scientist, these are some of the questions we can start thinking
about.
To answer these questions, we're going to need some data."""
# __author__ = "Pallavi Chandra"
# Project Name = "Data Analysis for Automobile dataset"
import pandas as pd

#Retrieve the data from the URL
data_path = "https://s3-api.us-geo.objectstorage.softlayer.net/cf-courses-data/CognitiveClass/DA0101EN/auto.csv"
#Read the csv data
df = pd.read_csv(data_path, header=None)
print(df.head(10))

#Define headers or columns from dataset description information is available at: https://archive.ics.uci.edu/ml/datasets/Automobile
headers = ["symboling","normalized-losses","make","fuel-type","aspiration", "num-of-doors","body-style",
         "drive-wheels","engine-location","wheel-base", "length","width","height","curb-weight","engine-type",
         "num-of-cylinders", "engine-size","fuel-system","bore","stroke","compression-ratio","horsepower",
         "peak-rpm","city-mpg","highway-mpg","price"]
print("headers\n", headers)

df.columns = headers
print(df.columns) # name of the column
print(df.head(10))
print(df.dtypes)
summary = df.describe()
print(summary.transpose())
print(summary.head())
print(df.info)

#we can drop missing values along the column "price" as follows
df.dropna(subset=["price"], axis=0)

#Save Dataset
df.to_csv("etc/automobile.csv", index=False)

#However, what if we would also like to check all the columns including those that are of type object.
#You can add an argument include = "all" inside the bracket
#describe all the columns in df including object

sum_incObj = df.describe(include="all")
print(sum_incObj.transpose())
sum_3cols = df[['fuel-type','num-of-doors','body-style']].describe()
print(sum_3cols.transpose())

#Data Wrangling
import matplotlib.pylab as plt

#How to work with missing data?

#Steps for working with missing data:
#dentify missing data
#deal with missing data
#correct data format

#Convert "?" to NaN In the car dataset, missing data comes with the question mark "?".
# We replace "?" with NaN (Not a Number), which is Python's default missing value marker

import numpy as np
#replace '?' to NaN
df.replace("?", np.nan, inplace=True)
print(df.head(10))

#Evaluating for missing data using isnull(), .notnull()
missing_data = df.isnull()
print(missing_data.head(n=10)) # returns a boolean value "True" means missing value and "False" means missing value.

#Count missing values in each column
for col in missing_data.columns.values.tolist():
    print("Column ",col)
    print("Missing data ",missing_data[col], " ",missing_data[col].value_counts())
    print(df.size)
    print(df.shape)
    print(df.ndim)

#Deal with missing data
#How to deal with missing data?
#drop data
#a. drop the whole row
#b. drop the whole column
#replace data
#a. replace it by mean
#b. replace it by frequency
#c. replace it based on other functions

#Whole columns should be dropped only if most entries in the column are empty. In our dataset, none of the columns are empty enough to drop entirely. We have some freedom in choosing which method to replace data; however, some methods may seem more reasonable than others. We will apply each method to many different columns:

#Replace by mean:

#"normalized-losses": 41 missing data, replace them with mean
#"stroke": 4 missing data, replace them with mean
#"bore": 4 missing data, replace them with mean
#"horsepower": 2 missing data, replace them with mean
#"peak-rpm": 2 missing data, replace them with mean
#Replace by frequency:
#"num-of-doors": 2 missing data, replace them with "four".
#Reason: 84% sedans is four doors. Since four doors is most frequent, it is most likely to occur
#Drop the whole row:
#"price": 4 missing data, simply delete the whole row
#Reason: price is what we want to predict. Any data entry without price data cannot be used for prediction; therefore any row now without price data is not useful to us"""

avg_norm_loss = df["normalized-losses"].astype("float").mean(axis=0)
print("Average of normalized-losses: ", avg_norm_loss)

#Replace NaN by mean value in "normalized-losses"
df["normalized-losses"].replace(np.nan,avg_norm_loss, inplace=True)

#Calculate the mean value of "bore" column
avg_bore = df['bore'].astype("float").mean(axis=0)
print("Average of bore: ", avg_bore)

#Replace NaN by mean value
df['bore'].replace(np.nan,avg_bore,inplace=True)

#Replace NaN for Stroke column by mean
avg_stroke = df['stroke'].astype("float").mean(axis=0)
print("Average of stroke: ", avg_stroke)
df['stroke'].replace(np.nan, avg_stroke, inplace=True)

#Calculate the mean value for the 'horsepower' column:
avg_hrspower = df['horsepower'].astype("float").mean(axis=0)
print("Average of horsepower: ", avg_hrspower)

#Replace NaN with mean/avg
df['horsepower'].replace(np.nan, avg_hrspower, inplace=True)

#Calculate the mean value for 'peak-rpm' column:
avg_peakrpm=df['peak-rpm'].astype('float').mean(axis=0)
print("Average peak rpm:", avg_peakrpm)

#Replace NaN by mean value:
df['peak-rpm'].replace(np.nan, avg_peakrpm, inplace=True)

#To see which values are present in a particular column, we can use the ".value_counts()" method:
print("Number of doors values: ",df['num-of-doors'].value_counts())

#We can see that four doors are the most common type. We can also use the ".idxmax()"
# method to calculate for us the most common type automatically
print("Most common type number of doors: ", df['num-of-doors'].value_counts().idxmax())

#The replacement procedure is very similar to what we have seen previously
#replace the missing 'num-of-doors' values by the most frequent
df['num-of-doors'].replace(np.nan, "four", inplace=True)

#Simply drop the whole row with NaN in "price" column
df.dropna(subset=["price"], axis=0, inplace=True)

#reset index, because we dropped two rows
df.reset_index(drop=True, inplace=True)

print(df.head(n=5))


#Correct data format
#We are almost there!
#The last step in data cleaning is checking and making sure that all data is in the
# correct format (int, float, text or other).
#In Pandas, we use
#.dtype() to check the data type
#.astype() to change the data type

df.dtypes

"""As we can see above, some columns are not of the correct data type. Numerical 
variables should have type 'float' or 'int', and variables with strings such as 
categories should have type 'object'. For example, 'bore' and 'stroke' variables 
are numerical values that describe the engines, so we should expect them to be of the 
type 'float' or 'int'; however, they are shown as type 'object'. We have to convert 
data types into a proper format for each column using the astype() method."""

df[["bore", "stroke"]] = df[["bore", "stroke"]].astype("float")
df[["normalized-losses"]] = df[["normalized-losses"]].astype("int")
df[["price"]]=df[["price"]].astype("float")
df[["peak-rpm"]]=df[["peak-rpm"]].astype("float")

print(df.dtypes)

#Data Standardization scaling to N(0,1)
# Convert mpg to L/100km by mathematical operation (235 divided by mpg)
df['city-L/100km'] = 235/df["city-mpg"]

# check your transformed data
print(df.head())

"""
Data Normalization¶
Why normalization?

Normalization is the process of transforming values of several variables into a similar range. Typical normalizations include scaling the variable so the variable average is 0, scaling the variable so the variance is 1, or scaling variable so the variable values range from 0 to 1

Example

To demonstrate normalization, let's say we want to scale the columns "length", "width" and "height"

Target:would like to Normalize those variables so their value ranges from 0 to 1.

Approach: replace original value by (original value)/(maximum value)
"""

#replace (original value) by (org val)/max val
df['length'] = df['length']/df['length'].max()
df['width'] = df['width']/df['width'].max()

"""
Binning
Why binning?
Binning is a process of transforming continuous numerical variables into discrete categorical 'bins', for grouped analysis.

Example:

In our dataset, "horsepower" is a real valued variable ranging from 48 to 288, it has 57 unique values. What if we only care about the price difference between cars with high horsepower, medium horsepower, and little horsepower (3 types)? Can we rearrange them into three ‘bins' to simplify analysis?

We will use the Pandas method 'cut' to segment the 'horsepower' column into 3 bins
"""

df['horsepower'] = df["horsepower"].astype(int, copy=True)

#Lets plot the histogram of horspower, to see what the distribution of horsepower looks like.
import matplotlib as plt
from matplotlib import pyplot
plt.pyplot.hist(df['horsepower'])

#set x/y labels and pot title
plt.pyplot.xlabel("horsepower")
plt.pyplot.ylabel("count")
plt.pyplot.title("horsepower bins")
plt.pyplot.show()

"""
We would like 3 bins of equal size bandwidth so we use numpy's linspace(start_value, end_value, numbers_generated function.

Since we want to include the minimum value of horsepower we want to set start_value=min(df["horsepower"]).

Since we want to include the maximum value of horsepower we want to set end_value=max(df["horsepower"]).

Since we are building 3 bins of equal length, there should be 4 dividers, so numbers_generated=4.
"""

bins = np.linspace(min(df['horsepower']), max(df['horsepower']),4)
bins


#We set group names:
group_names = ['Low', 'Medium', 'High']

#We apply the function "cut" to determine what each value of "df['horsepower']" belongs to.
df['horsepower-binned']=pd.cut(df['horsepower'], bins, labels=group_names, include_lowest=True)
#Lets see no. of vehicles in each bin
print(df['horsepower-binned'].value_counts())
print(df[['horsepower','horsepower-binned']].head(20))

plt.pyplot.bar(group_names,df['horsepower-binned'].value_counts())
plt.pyplot.xlabel("horsepower")
plt.pyplot.ylabel("count")
plt.pyplot.title("horsepower bins")
plt.pyplot.show()

# draw historgram of attribute "horsepower" with bins = 3
plt.pyplot.hist(df["horsepower"], bins = 3)
# set x/y labels and plot title
plt.pyplot.xlabel("horsepower")
plt.pyplot.ylabel("count")
plt.pyplot.title("horsepower bins")
plt.pyplot.show()


"""
Indicator variable (or dummy variable)
What is an indicator variable?
An indicator variable (or dummy variable) is a numerical variable used to label categories. They are called 'dummies' because the numbers themselves don't have inherent meaning.

Why we use indicator variables?

So we can use categorical variables for regression analysis in the later modules.

Example
We see the column "fuel-type" has two unique values, "gas" or "diesel". Regression doesn't understand words, only numbers. To use this attribute in regression analysis, we convert "fuel-type" into indicator variables.

We will use the panda's method 'get_dummies' to assign numerical values to different categories of fuel type.
"""

dummy_var1 = pd.get_dummies(df['fuel-type'])
print('dummay variable for fuel type: ', dummy_var1.head(20))

#Change colnames for clarity
dummy_var1.rename(columns={'fuel-type-diesel':'gas','fuel-type-diesel':'diesel'}, inplace=True)
print("After changing column names    ",dummy_var1.head())

#merge df and dummy variables
df = pd.concat([df, dummy_var1], axis=1)

#drop original col "fuel-type" from df
df.drop("fuel-type",axis=1,inplace=True)
print(df.head())

#Exploratory Data Analysis
import seaborn as sns

"""
Continuous numerical variables:
Continuous numerical variables are variables that may contain any value within some range. Continuous numerical variables can have the type "int64" or "float64". A great way to visualize these variables is by using scatterplots with fitted lines.

In order to start understanding the (linear) relationship between an individual variable and the price. We can do this by using "regplot", which plots the scatterplot plus the fitted regression line for the data.

Let's see several examples of different linear relationships:
"""

#scatterplot of "engine-size" and "price"
# Engine size as potential predictor variable of price
#sns.regplot(x="engine-size", y="price", data=df)

#correlation between 'engine-size' and 'price' and see it's approximately 0.87
print(df[["engine-size","price"]].corr())

#Highway mpg is a potential predictor variable of price
sns.regplot(x="highway-mpg", y="price", data=df)
df[['highway-mpg', 'price']].corr()

"""
Weak Linear Relationship
Let's see if "Peak-rpm" as a predictor variable of "price".
"""

sns.regplot(x="peak-rpm", y="price", data=df)

"""Peak rpm does not seem like a good predictor of the price at all since the 
regression line is close to horizontal. Also, the data points are very scattered 
and far from the fitted line, showing lots of variability. Therefore it's it is not 
a reliable variable."""

#examine the correlation between 'peak-rpm' and 'price' and see it's approximately -0.101616
df[['peak-rpm','price']].corr()

#Categorical variables
#These are variables that describe a 'characteristic' of a data unit, and are selected from a small group of
# categories. The categorical variables can have the type "object" or "int64". A good way to visualize categorical
# variables is by using boxplots.

#Let's look at the relationship between "body-style" and "price".
#sns.boxplot(x="body-style", y="price", data=df)

#We see that the distributions of price between the different body-style categories have a significant overlap, and
# so body-style would not be a good predictor of price. Let's examine engine "engine-location" and "price":
#sns.boxplot(x="engine-location", y="price", data=df)

# drive-wheels
#sns.boxplot(x="drive-wheels", y="price", data=df)

print("drive wheels: ",df['drive-wheels'].head(n=10))

df['drive-wheels'].unique()
df_group_one = df[['drive-wheels','body-style','price']]
df_group_one = df_group_one.groupby(['drive-wheels'], as_index=False).mean()
print(df_group_one)

#grouping results
df_gptest = df[['drive-wheels','body-style','price']]
grouped_test1 = df_gptest.groupby(['drive-wheels','body-style'],as_index=False).mean()
print(grouped_test1)

"""
This grouped data is much easier to visualize when it is made into a pivot table. A pivot table is like an Excel spreadsheet, 
with one variable along the column and another along the row. We can convert the dataframe to a pivot table using the method 
"pivot " to create a pivot table from the groups.In this case, we will leave the drive-wheel variable as the rows of the table, and pivot body-style to become the columns of the table:
"""

grouped_pivot = grouped_test1.pivot(index='drive-wheels', columns='body-style')
print(grouped_pivot)

"""
Often, we won't have data for some of the pivot cells. We can fill these missing cells with the value 0, but any other value
could potentially be used as well. It should be mentioned that missing data is quite a complex subject and is an entire course on its own.
"""
grouped_pivot = grouped_pivot.fillna(0) #fill missing values with 0
print("Pivot table(cells) with no NaNs: ",grouped_pivot)
#Often, we won't have data for some of the pivot cells. We can fill these missing cells with the value 0, but any other value
#  could potentially be used as well. It should be mentioned that missing data is quite a complex subject and is an entire
# course on its own.
#Let's use a heat map to visualize the relationship between Body Style vs Price.

#use the grouped results
plt.pyplot.pcolor(grouped_pivot, cmap='RdBu')
plt.pyplot.colorbar()
plt.pyplot.show()

"""The heatmap plots the target variable (price) proportional to colour with respect to the variables 'drive-wheel' and 
'body-style' in the vertical and horizontal axis respectively. 
This allows us to visualize how the price is related to 'drive-wheel' and 'body-style'."""

"""
5. Correlation and Causation
Correlation: a measure of the extent of interdependence between variables.

Causation: the relationship between cause and effect between two variables.

It is important to know the difference between these two and that correlation does not imply causation. Determining correlation is much simpler the determining causation as causation may require independent experimentation.

Pearson Correlation

The Pearson Correlation measures the linear dependence between two variables X and Y.

The resulting coefficient is a value between -1 and 1 inclusive, where:

1: Total positive linear correlation.
0: No linear correlation, the two variables most likely do not affect each other.
-1: Total negative linear correlation.
Pearson Correlation is the default method of the function "corr". Like before we can calculate the Pearson Correlation of the of the 'int64' or 'float64' variables
"""

"""
sometimes we would like to know the significant of the correlation estimate.

P-value:

What is this P-value? The P-value is the probability value that the correlation between these two variables is statistically significant. Normally, we choose a significance level of 0.05, which means that we are 95% confident that the correlation between the variables is significant.

By convention, when the

p-value is << 0.001: we say there is strong evidence that the correlation is significant.
the p-value is << 0.05: there is moderate evidence that the correlation is significant.
the p-value is << 0.1: there is weak evidence that the correlation is significant.
the p-value is >> 0.1: there is no evidence that the correlation is significant.
We can obtain this information using "stats" module in the "scipy" library.
"""
print("group the variable drive wheels: ",df['drive-wheels'].unique())
print("avg price for each of the differengt categories of drive wheels: ",df_group_one)
print("Grouping on drivewheels and body style: ",grouped_test1)
print("In Pivot table format: ", grouped_pivot)

#Pearson Correlation Coefficient and P-value of 'wheel-base' and 'price'.
from scipy import stats
pearson_coef, p_value = stats.pearsonr(df['wheel-base'], df['price'])
print("The Pearson Correlation Coefficient is ", pearson_coef, "with a p-value of P =", p_value)

"""
Conclusion:
Since the p-value is  <<  0.001, the correlation between wheel-base and price is statistically significant, although the linear
 relationship isn't extremely strong (~0.585)"""

#Let's calculate the Pearson Correlation Coefficient and P-value of 'horsepower' and 'price'.
pearson_coef, p_value = stats.pearsonr(df['horsepower'], df['price'])
print("The Pearson Correlation Coefficient is", pearson_coef, " with a P-value of P = ", p_value)

"""
Conclusion:
Since the p-value is  <<  0.001, the correlation between horsepower and price is statistically significant, and the 
linear relationship is quite strong (~0.809, close to 1)
"""

#Let's calculate the Pearson Correlation Coefficient and P-value of 'length' and 'price'.
pearson_coef, p_value = stats.pearsonr(df['length'], df['price'])
print("The Pearson Correlation Coefficient is", pearson_coef, " with a P-value of P = ", p_value)

"""Conclusion:
Since the p-value is  <<  0.001, the correlation between length and price is statistically significant, and the linear 
relationship is moderately strong (~0.691)."""

#Let's calculate the Pearson Correlation Coefficient and P-value of 'width' and 'price':

pearson_coef, p_value = stats.pearsonr(df['width'], df['price'])
print("The Pearson Correlation Coefficient is", pearson_coef, " with a P-value of P =", p_value )

"""
Conclusion:
Since the p-value is < 0.001, the correlation between width and price is statistically significant, and the linear 
relationship is quite strong (~0.751).

"""
#Let's calculate the Pearson Correlation Coefficient and P-value of 'curb-weight' and 'price':
pearson_coef, p_value = stats.pearsonr(df['curb-weight'], df['price'])
print( "The Pearson Correlation Coefficient is", pearson_coef, " with a P-value of P = ", p_value)
"""Conclusion:
Since the p-value is << 0.001, the correlation between curb-weight and price is statistically significant, and the 
linear relationship is quite strong (~0.834).
"""
#Let's calculate the Pearson Correlation Coefficient and P-value of 'engine-size' and 'price':
pearson_coef, p_value = stats.pearsonr(df['engine-size'], df['price'])
print("The Pearson Correlation Coefficient is", pearson_coef, " with a P-value of P =", p_value)
"""Conclusion:
Since the p-value is << 0.001, the correlation between engine-size and price is statistically significant, and the linear
 relationship is very strong (~0.872).
"""
#Let's calculate the Pearson Correlation Coefficient and P-value of 'bore' and 'price':
pearson_coef, p_value = stats.pearsonr(df['bore'], df['price'])
print("The Pearson Correlation Coefficient is", pearson_coef, " with a P-value of P =  ", p_value )
"""Conclusion:
Since the p-value is << 0.001, the correlation between bore and price is statistically significant, but the linear relationship is only moderate (~0.521).
"""
#We can relate the process for each 'City-mpg' and 'Highway-mpg':
pearson_coef, p_value = stats.pearsonr(df['city-mpg'], df['price'])
print("The Pearson Correlation Coefficient is", pearson_coef, " with a P-value of P = ", p_value)
"""Conclusion:
Since the p-value is << 0.001, the correlation between city-mpg and price is statistically significant, and the coefficient 
of ~ -0.687 shows that the relationship is negative and moderately strong.
"""
pearson_coef, p_value = stats.pearsonr(df['highway-mpg'], df['price'])
print( "The Pearson Correlation Coefficient is", pearson_coef, " with a P-value of P = ", p_value )

"""
Conclusion:
Since the p-value is < 0.001, the correlation between highway-mpg and price is statistically significant, and the coefficient
of ~ -0.705 shows that the relationship is negative and moderately strong.
"""

#ANOVA
"""
ANOVA: Analysis of Variance
The Analysis of Variance (ANOVA) is a statistical method used to test whether there are significant differences between the means of two or more groups. ANOVA returns two parameters:

F-test score: ANOVA assumes the means of all groups are the same, calculates how much the actual means deviate from the assumption, and reports it as the F-test score. A larger score means there is a larger difference between the means.

P-value: P-value tells how statistically significant is our calculated score value.

If our price variable is strongly correlated with the variable we are analyzing, expect ANOVA to return a sizeable F-test score and a small p-value.
"""
grouped_test2 = df_gptest[['drive-wheels', 'price']].groupby(['drive-wheels'])
grouped_test2.head(2)
print("",df_gptest)

#We can obtain the values of the method group using the method "get_group".
print("Get grouped value         ",grouped_test2.get_group('4wd')['price'])

#we can use the function 'f_oneway' in the module 'stats' to obtain the F-test score and P-value.
f_val, p_val = stats.f_oneway(grouped_test2.get_group('fwd')['price'],grouped_test2.get_group('rwd')['price'], grouped_test2.get_group('4wd')['price'])
print("ANOVA results: F=", f_val, ",P=",p_val)

print("ANOVA results: F=", f_val, " P=", p_val)

"""
This is a great result, with a large F test score showing a strong correlation and a P value of almost 0 
implying almost certain statistical significance. But does this mean all three tested groups are all this highly correlated?
"""

#Separately: fwd and rwd, 4wd and rwd, 4wd and fwd
f_val, p_val = stats.f_oneway(grouped_test2.get_group('fwd')['price'], grouped_test2.get_group('rwd')['price'])
print("ANOVA results: F=", f_val, ", P =", p_val)
f_val, p_val = stats.f_oneway(grouped_test2.get_group('4wd')['price'], grouped_test2.get_group('rwd')['price'])
print("ANOVA results: F=", f_val, ", P =", p_val)
f_val, p_val = stats.f_oneway(grouped_test2.get_group('fwd')['price'], grouped_test2.get_group('4wd')['price'])
print("ANOVA results: F=", f_val, ", P =", p_val)


"""
Conclusion: Important Variables
We now have a better idea of what our data looks like and which variables are important to take into account 
when predicting the car price. We have narrowed it down to the following variables:
Continuous numerical variables:
Length
Width
Curb-weight
Engine-size
Horsepower
City-mpg
Highway-mpg
Wheel-base
Bore
Categorical variables:
Drive-wheels
As we now move into building machine learning models to automate our analysis, feeding the model with variables
 that meaningfully affect our target variable will improve our model's prediction performance.
"""


#Module 4: Model development





















































































































