#!/usr/bin/env python
# coding: utf-8

# # <span style="font-width:bold; font-size: 3rem; color:#1EB182;"><img src="../../images/icon102.png" width="38px"></img> **Hopsworks Feature Store** </span><span style="font-width:bold; font-size: 3rem; color:#333;">- Part 02: Feature Pipeline</span>
# 
# [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/logicalclocks/hopsworks-tutorials/blob/master/advanced_tutorials/{project_name}/{notebook_name}.ipynb)
# 
# 
# ## 🗒️ This notebook is divided into the following sections:
# 1. Parse Data
# 2. Feature Group Insertion

# ## <span style='color:#ff5f27'> 📝 Imports

# In[1]:


import pandas as pd
from datetime import datetime
import time 
import requests

from functions import *


# ---

# ## <span style='color:#ff5f27'> 👮🏻‍♂️ API Keys

# ### Don't forget to create an `.env` configuration file where all the necessary environment variables (API keys) will be stored:
# ![](images/api_keys_env_file.png)

# In[2]:


date_today = datetime.now().strftime("%Y-%m-%d")


# ---

# ## <span style='color:#ff5f27'>  🧙🏼‍♂️ Parsing Data

# In[3]:


#cities = ['Kyiv', 'Stockholm', 'Sundsvall', 'Malmo']

data_air_quality = [get_air_quality_data()] #获得当天赫尔辛基的空气质量
data_weather = [get_weather_data(date_today)]


# ---

# ## <span style='color:#ff5f27'> 🧑🏻‍🏫 Dataset Preparation

# #### <span style='color:#ff5f27'> 👩🏻‍🔬 Air Quality Data

# In[4]:


df_air_quality = get_air_quality_df(data_air_quality)

df_air_quality


# In[5]:


df_air_quality.drop(['iaqi_h', 'iaqi_p', 'iaqi_pm10', 'iaqi_t', 'o3_max', 'o3_min', 'pm10_max', 'pm10_min', 'pm25_max', 'pm25_min', 'uvi_avg', 'uvi_max', 'uvi_min'], axis=1, inplace=True)


# In[6]:


df_air_quality


# In[7]:


# df_air_quality.rename(columns=['aqi','date', 'o3', 'pm10', 'pm25'], axis=1, inplace = True)
# df_air_quality


# #### <span style='color:#ff5f27'> 🌦 Weather Data

# In[8]:


df_weather = get_weather_df(data_weather)

df_weather.head()


# In[ ]:


#df_weather.drop([], axis=1, inplace=True)


# ---

# ## <span style="color:#ff5f27;"> 🔮 Connecting to Hopsworks Feature Store </span>

# In[20]:


import hopsworks

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


# In[ ]:


df_air_quality


# ---

# ## <span style="color:#ff5f27;">⬆️ Uploading new data to the Feature Store</span>

# In[26]:


air_quality_fg.insert(df_air_quality)


# In[ ]:


weather_fg.insert(df_weather)


# ---
