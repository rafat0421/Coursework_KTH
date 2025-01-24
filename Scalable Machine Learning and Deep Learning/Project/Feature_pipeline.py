import pandas as pd
from datetime import datetime
import time 
import requests
import hopsworks
from functions import *


date_today = datetime.now().strftime("%Y-%m-%d")

#cities = ['Kyiv', 'Stockholm', 'Sundsvall', 'Malmo']

data_air_quality = [get_air_quality_data()] 
data_weather = [get_weather_data(date_today)]

df_air_quality = get_air_quality_df(data_air_quality)

#df_air_quality

df_air_quality.drop(['iaqi_h', 'iaqi_p', 'iaqi_pm10', 'iaqi_t', 'o3_max', 'o3_min', 'pm10_max', 'pm10_min', 'pm25_max', 'pm25_min', 'uvi_avg', 'uvi_max', 'uvi_min'], axis=1, inplace=True)


# df_air_quality.rename(columns=['aqi','date', 'o3', 'pm10', 'pm25'], axis=1, inplace = True)
# df_air_quality

df_weather = get_weather_df(data_weather)

df_weather.head()

#df_weather.drop([], axis=1, inplace=True)

project = hopsworks.login()

fs = project.get_feature_store() 

air_quality_fg = fs.get_or_create_feature_group(
    name = 'air1_fg',
    primary_key = ['date'],
    version = 1
)
weather_fg = fs.get_or_create_feature_group(
   name = 'weather_fg',
   version = 1
)

air_quality_fg.insert(df_air_quality)

weather_fg.insert(df_weather)

