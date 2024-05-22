# -*- coding: utf-8 -*-
"""
Created on Fri Dec 11 18:58:17 2020

@author: Geet Chawla
"""


import pandas as pd
import numpy as np
import seaborn as sns
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import StratifiedKFold
from sklearn.tree import DecisionTreeClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from matplotlib import pyplot as plt
from sklearn.metrics import mean_squared_log_error
from sklearn.ensemble import RandomForestRegressor
#from lightgbm import LGBMRegressor


#One needs to change this only to store graphs / plots.
from os import chdir, getcwd
wd= r'***********' #Please input path to current working directory here
chdir(wd)

### Scraping the Trips data and making it ready to be merged

def scrape_data(url, name):
    data = pd.read_csv(url)
    
    #To check the dimensions of our data
    print(data.shape)
    
    #To check how our data looks
    print(data.head())
    
    #To check col types
    print(data.dtypes)
    
    data.to_csv(name+ ".csv")
    
    return data

trips = scrape_data("https://data.bts.gov/resource/w96p-f2qv.csv?$limit=2232606", "trips")

### Cleaning Trips Data

def trips_cleaning(data):
    data["date"] = pd.to_datetime(data["date"])
    data = data[(data['date'] > '2020-03-01')]
    data = data[['level','date','state_fips','pop_stay_at_home', 'pop_not_stay_at_home', 'trips']]
    data= data.replace(',','', regex=True)
    
    #To check how our data looks
    print(data.head())
    
    #To check the data types of each col
    print(data.dtypes)
    
    return data
    
trips_clean = trips_cleaning(trips)

### Using the data scraping function to scrape COVID Data

covid = scrape_data('https://api.covidtracking.com/v1/states/daily.csv', "raw_covid")


def covid_cleaning(data):
    data = data[['date', 'state', 'positiveIncrease']]
    data['date'] = pd.to_datetime(data['date'], format='%Y%m%d', errors='coerce')
    data['positiveIncrease'] = data['positiveIncrease'].astype(float)
    
    #To check how our data looks
    print(data.head())
    
    #To check the data types of each col
    print(data.dtypes)
    
    return data

covid_clean = covid_cleaning(covid)

### Scraping third from our function to get FIPS and State Codes to merge the previous two datasets

fips = scrape_data('https://raw.githubusercontent.com/fitnr/addfips/master/src/addfips/data/states.csv', "fips")


### Making Final Dataset for Analyses

final = []
def final_data(data, name):
    
    data = trips_clean.merge(fips, left_on='state_fips', right_on='fips')
    data = pd.merge(data, covid_clean, how = 'inner', left_on = ['postal','date'], right_on = ['state','date'])
    data = data[(data['level']=="State")]
    data = data[['date', 'fips', 'pop_stay_at_home', 'pop_not_stay_at_home', 'trips','name','positiveIncrease']]
    
    #To check how our data looks
    print(data.head())
    
    #To check the data types of each col
    print(data.dtypes)
    
    data.to_csv(name+ ".csv")
    
    return data

### Creating a function that adds metrics to normalize data for easier comparison across states

def normalize(data):
    data['population'] = data['pop_stay_at_home'] + data['pop_not_stay_at_home']
    data['trips_per_capita'] = data['trips'] / data['population']
    data['at_home_percent'] = data['pop_stay_at_home'] / data['population']
    data['not_at_home_percent'] = data['pop_not_stay_at_home'] / data['population']
    data['positivity_rate']  = data['positiveIncrease'] / data['population']
    
    #To check how our data looks
    print(data.head())
    
    #To check the data types of each col
    print(data.dtypes)
    
    return data

final = final_data(final, "final")

final = normalize(final)


### Making Summary Plot Functions - the follwoing function take inputs of FIPS codes and gives out 4 plots for that respective state:
### 1. COVID Trends across time in that State
### 2. COVID Positivity Rate across time in that State
### 3. Percent of population staying at home 
### 4. Percent of population not staying at home 

plt.rcParams['figure.figsize'] = [16, 8]
def basic_plots(fips_code, name):
    test = final[(final['fips']==fips_code)]
    
    f, (ax1, ax2)= plt.subplots(1, 2)
    ax1.set_title('COVID Trends Across Time')
    ax1.plot(test['date'], test[['positiveIncrease']], '-')
    ax1.tick_params(axis='x', labelrotation=90)
    ax1.axvline(pd.Timestamp('2020-11-13'),color='r', linestyle='--')
    ax1.text(x=pd.Timestamp('2020-11-5'), y=0, s='Thanksgiving Travel', alpha=0.7, color='r',rotation=90)
    ax1.set_ylabel('Number of COVID Cases')
    
    
    ax2.set_title('COVID Positivity Rate Across Time')
    ax2.plot(test['date'], test[['positivity_rate']], '-')
    ax2.tick_params(axis='x', labelrotation=90)
    ax2.axvline(pd.Timestamp('2020-11-13'),color='r', linestyle='--')
    ax2.text(x=pd.Timestamp('2020-11-5'), y=0, s='Thanksgiving Travel', alpha=0.7, color='r',rotation=90)
    ax2.set_ylabel('Number of COVID Cases per capita')
    
    plt.savefig(name+ "1.png")
    
    f, (ax3, ax4)= plt.subplots(1, 2)
    ax3.set_title('Population at Home')
    ax3.plot(test['date'], test[['at_home_percent']], '-')
    ax3.tick_params(axis='x', labelrotation=90)
    ax3.axvline(pd.Timestamp('2020-11-13'),color='r', linestyle='--')
    ax3.text(x=pd.Timestamp('2020-08-25'), y=0.2, s='Thanksgiving Travel', alpha=0.7, color='r')
    ax3.set_ylabel('Percentage of total population at home')
    
    ax4.set_title('Population Not at Home')
    ax4.plot(test['date'], test[['not_at_home_percent']], '-')
    ax4.tick_params(axis='x', labelrotation=90)
    ax4.axvline(pd.Timestamp('2020-11-13'),color='r', linestyle='--')
    ax4.text(x=pd.Timestamp('2020-08-25'), y=0.8, s='Thanksgiving Travel', alpha=0.7, color='r')
    ax4.set_ylabel('Percentage of total population not at home')
    
    plt.savefig(name+ "2.png")
    
    plt.show() 
    
#We can input any FIPS code and get the outputs for those states - here, we have tried for FIPS 38
basic_plots(38, "North Dakota")

#We now use a machine learning model to predict the number of COVID cases in the validatoin set, add a column in the 
#validation set and reuturn the new validation set with predictions

def machine_learning(data):
    train = data[data['date'] < '2020-09-01']
    val = data[data['date']  >= '2020-09-01']
    
    train_clean = train.drop(['date', 'name'], axis=1)
    val_clean = val.drop(['date','name'], axis = 1)
    
    xtr, xts = train_clean.drop(['positiveIncrease'], axis=1), val_clean.drop(['positiveIncrease'], axis=1)
    ytr, yts = train_clean['positiveIncrease'].values, val_clean['positiveIncrease'].values
    
    mdl = RandomForestRegressor(n_estimators=1000, random_state=0)
    mdl.fit(xtr, ytr)
    
    val['prediction'] = mdl.predict(xts)
    
    val.head()
    
    return val

validation = machine_learning(final)

#We now build a function to plot the predictions across the actual COVID cases - this would also have an annotation
#defining a proxy of Thanksgiving related holiday Travel that may have spiked cases

def prediction_plots(fips_code, name):
    test = validation[(validation['fips']==fips_code)]
    
    plt.plot(test['date'], test[['positiveIncrease']], '-', color = 'b')
    plt.plot(test['date'], test[['prediction']], '-', color = 'green')
    plt.tick_params(axis='x', labelrotation=45)
    plt.axvline(pd.Timestamp('2020-11-13'),color='r', linestyle='--')
    plt.text(x=pd.Timestamp('2020-11-14'), y=1000, s='Thanksgiving Travel', alpha=0.7, color='r',rotation=90)
    plt.ylabel('Number of COVID Cases')
    plt.xlabel('Date')
    plt.title('COVID Trends Across Time vs. Predicted Trends')
    plt.legend(('Actual Number of Covid Cases', 'Predicted Covid Cases'))
    
    plt.savefig(name+ ".png")
    
    plt.show() 

#The function takes FIPS code as the inputs and users are encouraged to play around with any FIPS code they may like
    
prediction_plots(17, "illinois")

