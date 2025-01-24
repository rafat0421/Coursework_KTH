#!/usr/bin/env python
# coding: utf-8

# # <span style="font-width:bold; font-size: 3rem; color:#1EB182;"><img src="../../images/icon102.png" width="38px"></img> **Hopsworks Feature Store** </span><span style="font-width:bold; font-size: 3rem; color:#333;">- Part 01: Backfill Features to the Feature Store</span>
# 
# [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/logicalclocks/hopsworks-tutorials/blob/master/advanced_tutorials/{project_name}/{notebook_name}.ipynb)
# 
# 
# ## 🗒️ This notebook is divided into the following sections:
# 1. Fetch historical data
# 2. Connect to the Hopsworks feature store
# 3. Create feature groups and insert them to the feature store
# 
# ![tutorial-flow](../../images/01_featuregroups.png)

# ## <span style='color:#ff5f27'> 📝 Imports

# In[1]:


import pandas as pd

from functions import *


# In[2]:


import csv
# with open('hel.csv') as in_file:
#     with open('output.csv', 'w', newline='') as out_file:
#         writer = csv.writer(out_file)
#         for row in csv.reader(in_file):
#             if any(field.strip() for field in row):
#                 writer.writerow(row)


# ---

# ## <span style='color:#ff5f27'> 💽 Loading Historical Data</span>
# 

# #### <span style='color:#ff5f27'> 👩🏻‍🔬 Air Quality Data

# In[3]:


df_air_quality = pd.read_csv('hela.csv')
df_air_quality.head()


# In[4]:


df_air_quality.date = df_air_quality.date.apply(timestamp_2_time)
df_air_quality.sort_values(by = ['date'],inplace = True,ignore_index = True)

df_air_quality.head()


# #### <span style='color:#ff5f27'> 🌦 Weather Data

# In[2]:


df_weather = pd.read_csv('helw.csv')

df_weather.tail(50)


# In[3]:


df_weather.date = df_weather.date.apply(timestamp_2_time1)
df_weather.sort_values(by=['date'],inplace=True, ignore_index=True)

df_weather.tail(50)


# ---

# ## <span style="color:#ff5f27;"> 🔮 Connecting to Hopsworks Feature Store </span>

# In[10]:


import hopsworks

project = hopsworks.login()

fs = project.get_feature_store() 


# ---

# ## <span style="color:#ff5f27;">🪄 Creating Feature Groups</span>

# #### <span style='color:#ff5f27'> 👩🏻‍🔬 Air Quality Data

# In[11]:


air_quality_fg = fs.get_or_create_feature_group(
        name = 'hel_air_modal',
        description = 'Air Quality characteristics of each day',
        version = 1,
        primary_key = ['date'],
        online_enabled = True,
        event_time = 'date'
    )    

air_quality_fg.insert(df_air_quality)


# #### <span style='color:#ff5f27'> 🌦 Weather Data

# In[12]:


weather_fg = fs.get_or_create_feature_group(
        name = 'hel_weather_modal',
        description = 'Weather characteristics of each day',
        version = 1,
        primary_key = ['date'],
        online_enabled = True,
        event_time = 'date'
    )    

weather_fg.insert(df_weather)


# ---
