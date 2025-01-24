import pandas as pd
from datetime import datetime
from functions import *

date_today = datetime.now().strftime("%Y-%m-%d")

data_air_quality = [get_air_quality_data("London")]
data_weather = [get_weather_data("London", date_today)]

df_air_quality = get_air_quality_df(data_air_quality)

df_air_quality.head()

df_weather = get_weather_df(data_weather)

df_weather = df_weather.drop(columns=["feelslikemin", "feelslikemax", "precipprob", "snowdepth", "uvindex"])

import hopsworks
project = hopsworks.login()

fs = project.get_feature_store() 

air_quality_fg = fs.get_or_create_feature_group(
    name = 'hel_air_quality_fg',
    primary_key = ['date'],
    version = 1
)
air_quality_fg.insert(df_air_quality)

weather_fg = fs.get_or_create_feature_group(
   name = 'hel_weather_fg',
    primary_key = ['city', 'date'],
   version = 1
)
weather_fg.insert(df_weather)
