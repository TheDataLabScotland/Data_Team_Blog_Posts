# Created using Python 2.7.11
import pandas as pd
import numpy as np


data = pd.read_excel(r"C:\Users\Richard\Documents\Work\Blog\Data_Team_Blog_Posts\PepsiChallenge\Pepsi Challenge Results.xlsx")
print data

print len(data)
print data.shape

data_sorted = data.sort_values(by='Name', axis=0, ascending=True)
print data_sorted

data_filtered = data[(data['Gender']=="M") & (data['First']=="A")]
print data_filtered

print data['Which'].unique()

print data['Conf'].mean(axis=0)

print data['Gender'].value_counts()

data['Correct'] = np.where(data['Which']=="A", "Yes", "No")
print data

data_crosstable = pd.crosstab(data['First'], data['Act_Pref'])
print data_crosstable

data_pivot_table = data.pivot_table(values="Conf", index="Gender", columns="Gen_Pref", aggfunc=np.mean)
print data_pivot_table

