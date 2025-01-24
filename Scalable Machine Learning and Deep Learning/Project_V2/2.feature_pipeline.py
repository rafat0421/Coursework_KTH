import pandas as pd
from datetime import datetime
import time 
import requests
import hopsworks
from functions import *

date_today = datetime.now().strftime("%Y-%m-%d")

cities = ['Helsinki']

#data_air_quality = [get_air_quality_data(city) for city in cities]
#data_weather = [get_weather_data(city, date_today) for city in cities]

data_air_quality = [get_air_quality_data()]
data_weather = [get_weather_data(date_today)]

df_air_quality = get_air_quality_df(data_air_quality)
df_air_quality

df_weather = get_weather_df(data_weather)
df_weather.head()

project = hopsworks.login()
fs = project.get_feature_store() 

air_quality_fg = fs.get_or_create_feature_group(
    name = 'air_quality_fg',
    version = 1
)
weather_fg = fs.get_or_create_feature_group(
    name = 'weather_fg',
    version = 1
)

air_quality_fg.insert(df_air_quality)
weather_fg.insert(df_weather)