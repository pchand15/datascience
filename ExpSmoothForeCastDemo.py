"""Description:This  program uses Statmodel package to create the time series forcast for the House price data """
# __author__ = "Pallavi Chandra"
# Project Name = "Exponential Smoothing Forecasting"

#Import Stat analysis packages
from statsmodels.tsa.api import ExponentialSmoothing, SimpleExpSmoothing
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

#create a data frame from the csv file
df = pd.read_csv("kc_house_data.csv")
print (df.head(20))
print(df.dtypes)

#Create a timeseries data using column Date and price
df_4_dm = df[['date','price']]
print(df_4_dm.head(10))

#Convert the timestamp of date into the datetime format to do a plot to see price fluctuation
df['date'] = pd.to_datetime(df['date'])
print (df['date'].head(10))

#Plot the date and price to see the price fluctuation
df_4_dm.plot.line(x='date',y='price')
#plt.show()
df_4_dm.info()
print (np.round(df_4_dm.describe()))
print(type(df_4_dm['price']))
df_4_dm['price']=df_4_dm.price.astype(int)
df_4_dm.info()
#Based on the graph of price fluctuation, let us use optimized=TRUE to chose optimal value of alpha
fcast_pr = SimpleExpSmoothing(np.asarray(df_4_dm['price'])).fit()
fcast_val = fcast_pr.forecast(len(df_4_dm))
#Plot
plt.plot(df_4_dm['price'],label='Price')
plt.plot(fcast_val,label='y-hat value')
plt.show()









