#!/usr/bin/env python
# coding: utf-8

# # <span style="font-width:bold; font-size: 3rem; color:#1EB182;"><img src="../../images/icon102.png" width="38px"></img> **Hopsworks Feature Store** </span><span style="font-width:bold; font-size: 3rem; color:#333;">- Part 04: Batch Predictions</span>
# 
# [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/logicalclocks/hopsworks-tutorials/blob/master/advanced_tutorials/{project_name}/{notebook_name}.ipynb)
# 
# 
# ## 🗒️ This notebook is divided into the following sections:
# 
# 1. Loading the training data
# 2. Train the model
# 3. Register model in Hopsworks model registry
# 
# ![part3](../../images/03_model.png) 

# ### <span style='color:#ff5f27'> 📝 Imports

# In[1]:


import pandas as pd

from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import f1_score

import warnings
warnings.filterwarnings("ignore")


# ---

# ## <span style="color:#ff5f27;"> 📡 Connecting to Hopsworks Feature Store </span>

# In[2]:


import hopsworks

project = hopsworks.login() 

fs = project.get_feature_store() 


# ---

# ## <span style="color:#ff5f27;">🪝 Feature View and Training Dataset Retrieval</span>

# In[3]:


feature_view = fs.get_feature_view(
    name = 'hel_air_fv1',
    version = 1
)


# In[4]:


train_data = feature_view.get_training_data(1)[0]

train_data.head()


# ---

# ## <span style="color:#ff5f27;">🧬 Modeling</span>

# In[5]:


train_data = train_data.sort_values(by=["date", 'city'], ascending=[False, True]).reset_index(drop=True)
train_data["aqi_next_day"] = train_data.groupby('city')['aqi'].shift(1)

train_data.head(5)


# In[6]:


X = train_data.drop(columns=["date"]).fillna(0)
y = X.pop("aqi_next_day")


# In[7]:


gb = GradientBoostingRegressor()
gb.fit(X, y)


# ### <span style='color:#ff5f27'> 👨🏻‍⚖️ Model Validation

# In[8]:


f1_score(y.astype('int'),[int(pred) for pred in gb.predict(X)],average='micro')


# In[9]:


y.iloc[4:10].values


# In[10]:


pred_df = pd.DataFrame({
    'aqi_real': y.iloc[4:10].values,
    'aqi_pred': map(int, gb.predict(X.iloc[4:10]))
}
)
pred_df


# ---

# ## <span style='color:#ff5f27'>🗄 Model Registry</span>
# 
# One of the features in Hopsworks is the model registry. This is where you can store different versions of models and compare their performance. Models from the registry can then be served as API endpoints.

# In[11]:


mr = project.get_model_registry()


# ### <span style="color:#ff5f27;">⚙️ Model Schema</span>

# The model needs to be set up with a [Model Schema](https://docs.hopsworks.ai/machine-learning-api/latest/generated/model_schema/), which describes the inputs and outputs for a model.
# 
# A Model Schema can be automatically generated from training examples, as shown below.

# In[12]:


from hsml.schema import Schema
from hsml.model_schema import ModelSchema

input_schema = Schema(X)
output_schema = Schema(y)
model_schema = ModelSchema(input_schema=input_schema, output_schema=output_schema)

model_schema.to_dict()


# In[13]:


import joblib

joblib.dump(gb, 'model.pkl')


# In[14]:


model = mr.sklearn.create_model(
    name="gradient_boost_model",
    metrics={"f1": "0.5"},
    description="Gradient Boost Regressor.",
    input_example=X.sample(),
    model_schema=model_schema
)

model.save('model.pkl')


# In[ ]:




