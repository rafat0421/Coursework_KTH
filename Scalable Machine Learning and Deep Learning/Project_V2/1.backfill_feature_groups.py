import pandas as pd
from functions import *
import csv
import hopsworks


# with open('hel.csv') as in_file:
#     with open('output.csv', 'w', newline='') as out_file:
#         writer = csv.writer(out_file)
#         for row in csv.reader(in_file):
#             if any(field.strip() for field in row):
#                 writer.writerow(row)



df_air_quality = pd.read_csv('hela.csv')
#df_air_quality.head()

df_air_quality.date = df_air_quality.date.apply(timestamp_2_time)
df_air_quality.sort_values(by = ['date'],inplace = True,ignore_index = True)

#df_air_quality.head()
df_weather = pd.read_csv('helw.csv')

#df_weather.tail(50)

df_weather.date = df_weather.date.apply(timestamp_2_time1)
df_weather.sort_values(by=['date'],inplace=True, ignore_index=True)

#df_weather.tail(50)

project = hopsworks.login()

fs = project.get_feature_store() 

air_quality_fg = fs.get_or_create_feature_group(
        name = 'air_quality_fg',
        description = 'Air Quality characteristics of each day',
        version = 1,
        primary_key = ['city','date'],
        online_enabled = True,
        event_time = 'date'
    )    

air_quality_fg.insert(df_air_quality)    

weather_fg = fs.get_or_create_feature_group(
        name = 'weather_fg',
        description = 'Weather characteristics of each day',
        version = 1,
        primary_key = ['city','date'],
        online_enabled = True,
        event_time = 'date'
    )    

weather_fg.insert(df_weather)