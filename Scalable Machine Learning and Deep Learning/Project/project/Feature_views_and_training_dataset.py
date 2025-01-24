#!/usr/bin/env python
# coding: utf-8

# # <span style="font-width:bold; font-size: 3rem; color:#1EB182;"><img src="../../images/icon102.png" width="38px"></img> **Hopsworks Feature Store** </span><span style="font-width:bold; font-size: 3rem; color:#333;">- Part 03: Training Data & Feature views</span>
# 
# [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/logicalclocks/hopsworks-tutorials/blob/master/advanced_tutorials/{project_name}/{notebook_name}.ipynb)
# 
# <span style="font-width:bold; font-size: 1.4rem;">This notebook explains how to read from a feature group and create training dataset within the feature store</span>
# 
# ## 🗒️ This notebook is divided into the following sections:
# 
# 1. Fetch Feature Groups
# 2. Define Transformation functions
# 4. Create Feature Views
# 5. Create Training Dataset with training, validation and test splits
# 
# ![part2](../../images/02_training-dataset.png) 

# ## <span style="color:#ff5f27;"> 📡 Connecting to Hopsworks Feature Store </span>

# In[2]:


import hopsworks

project = hopsworks.login()

fs = project.get_feature_store() 


# In[3]:


air_quality_fg = fs.get_or_create_feature_group(
    name = 'hel_air_modal',
    version = 1
)
weather_fg = fs.get_or_create_feature_group(
    name = 'hel_weather_modal',
    version = 1
)


# In[4]:


query = air_quality_fg.select_all().join(weather_fg.select_all())


# In[5]:


query.read()


# --- 
# 
# ## <span style="color:#ff5f27;"> 🖍 Feature View Creation and Retrieving </span>

# In[6]:


query = air_quality_fg.select_all().join(weather_fg.select_all())

query_show = query.show(5)
col_names = query_show.columns

query_show


# ### <span style="color:#ff5f27;">🧑🏻‍🔬 Transformation functions</span>
# 
# Hopsworks Feature Store provides functionality to attach transformation functions to training datasets.
# 
# Hopsworks Feature Store also comes with built-in transformation functions such as `min_max_scaler`, `standard_scaler`, `robust_scaler` and `label_encoder`.

# In[7]:


[t_func.name for t_func in fs.get_transformation_functions()]


# You can retrieve transformation function you need.
# 
# To attach transformation function to training dataset provide transformation functions as dict, where key is feature name and value is online transformation function name.
# 
# Also training dataset must be created from the Query object. Once attached transformation function will be applied on whenever save, insert and get_serving_vector methods are called on training dataset object.

# In[12]:


category_cols = ['city', 'date','conditions','aqi']

mapping_transformers = {col_name:fs.get_transformation_function(name='standard_scaler') for col_name in col_names if col_name not in category_cols}
category_cols = {col_name:fs.get_transformation_function(name='label_encoder') for col_name in category_cols if col_name not in ['date','aqi']}

mapping_transformers.update(category_cols)


# `Feature Views` stands between **Feature Groups** and **Training Dataset**. Сombining **Feature Groups** we can create **Feature Views** which store a metadata of our data. Having **Feature Views** we can create **Training Dataset**.
# 
# The Feature Views allows schema in form of a query with filters, define a model target feature/label and additional transformation functions.
# 
# In order to create Feature View we can use `FeatureStore.create_feature_view()` method.
# 
# You can specify next parameters:
# 
# - `name` - name of a feature group.
# 
# - `version` - version of a feature group.
# 
# - `labels`- our target variable.
# 
# - `transformation_functions` - functions to transform our features.
# 
# - `query` - query object with data.

# In[13]:


feature_view = fs.create_feature_view(
    name = 'hel_air_fv1',
    version = 1,
    transformation_functions = mapping_transformers,
    query = query
)


# For now `Feature View` is saved in Hopsworks and you can retrieve it using `FeatureStore.get_feature_view()`.

# In[14]:


feature_view = fs.get_feature_view(
    name = 'hel_air_fv1',
    version = 1
)


# In[15]:


feature_view.create_training_data()


# ---
# 
# ## <span style="color:#ff5f27;"> 🏋️ Training Dataset Creation</span>
# 
# In Hopsworks training data is a query where the projection (set of features) is determined by the parent FeatureView with an optional snapshot on disk of the data returned by the query.
# 
# **Training Dataset  may contain splits such as:** 
# * Training set - the subset of training data used to train a model.
# * Validation set - the subset of training data used to evaluate hparams when training a model
# * Test set - the holdout subset of training data used to evaluate a mode
# 
# To create training dataset you use `FeatureView.create_training_data()` method.
# 
# Here are some importand things:
# 
# - It will inherit the name of FeatureView.
# 
# - The feature store currently supports the following data formats for
# training datasets: **tfrecord, csv, tsv, parquet, avro, orc**.
# 
# - You can choose necessary format using **data_format** parameter.
# 
# - **start_time** and **end_time** in order to filter dataset in specific time range.

# ## <span style="color:#ff5f27;">⏭️ **Next:** Part 04 </span>
# 
# In the next notebook you will train a model on the dataset, that was created in this notebook.
