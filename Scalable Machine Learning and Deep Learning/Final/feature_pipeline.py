import pandas as pd
import numpy as np
from datetime import datetime
import time 
import requests

from functions import *
date_today = datetime.now().strftime("%Y-%m-%d")
import hopsworks

project = hopsworks.login()

fs = project.get_feature_store() 

#ay3JLXxqldRqwVJo.zxFgix0ek7ifTbNxbzB4VJMBLer9TK5xNe8IC0PNRA1rNIqeybLI3uwZRhURvSJI
# upload dataset for feature group
weather_fg = fs.get_or_create_feature_group(
    name = 'weather_fg',
    version = 1
)
#weather_fg.delete()
weather_fg = fs.get_or_create_feature_group(
        name = 'weather_fg',
        description = 'Weather characteristics of each day',
        version = 1,
        primary_key = ['datetime'],
        online_enabled = True,
        event_time = 'datetime'
    )    
df_weather = pd.read_csv('https://raw.githubusercontent.com/ponyyuhan/ID2223_project/main/shanghai%202020-04-19%20to%202022-12-12.csv')

df_weather

print(df_weather)

df_weather.datetime = df_weather.datetime.apply(timestamp_2_time)
#df_weather.sort_values(by=['city', 'datetime'], inplace = True, ignore_index = True)
df_weather.drop('preciptype', inplace = True, axis = 1)
#df_weather['severerisk'] = df_weather['severerisk'].replace(np.nan, 0)
df_weather.drop('severerisk', inplace = True, axis = 1)
df_weather.drop('stations', inplace = True, axis = 1)
#df_weather['datetime'] = pd.to_da(df_weather['datetime'])
df_weather.drop('sunrise', inplace = True, axis = 1)
#df_weather.drop('sunriseEpoch', inplace = True, axis = 1)
df_weather.drop('sunset', inplace = True, axis = 1)
#df_weather.drop('sunsetepoch', inplace = True, axis = 1)
df_weather.drop('moonphase', inplace = True, axis = 1)
df_weather.drop('description', inplace = True, axis = 1)
df_weather.drop('icon', inplace = True, axis = 1)
weather_fg.insert(df_weather, write_options={"wait_for_job" : False})
# Get today's weather report
from datetime import datetime
import time
import requests

from functions import *
date_today = datetime.now().strftime("%Y-%m-%d")

cities = ['shanghai']
data_weather_today = [get_weather_data(city, date_today) for city in cities]

df_weather_today = get_weather_df(data_weather_today)
df_weather_today['uvindex'] = df_weather_today['uvindex'].astype('int64')
df_weather_today['precipprob'] = df_weather_today['precipprob'].astype('int64')
weather_fg = fs.get_or_create_feature_group(
    name = 'weather_fg',
    version = 1
)
weather_fg.insert(df_weather_today)
