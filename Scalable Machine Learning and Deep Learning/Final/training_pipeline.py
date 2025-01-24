!pip install hopsworks
!pip install python-dotenv
!pip install joblib
import pandas as pd
import numpy as np
from datetime import datetime
import time 
import requests
import hopsworks

project = hopsworks.login()

fs = project.get_feature_store()

#ay3JLXxqldRqwVJo.zxFgix0ek7ifTbNxbzB4VJMBLer9TK5xNe8IC0PNRA1rNIqeybLI3uwZRhURvSJI
weather_fg = fs.get_or_create_feature_group(
    name = 'weather_fg',
    version = 1
)

query = weather_fg.select_all()
query.read()
# Get columns' name
query_show =query.show(5)
col_names= query_show.columns

# Data scaling
category_cols = ['name','datetime','conditions', 'tempmin', 'tempmax', 'temp']

mapping_transformers = {col_name:fs.get_transformation_function(name='standard_scaler') for col_name in col_names if col_name not in category_cols}
category_cols = {col_name:fs.get_transformation_function(name='label_encoder') for col_name in category_cols if col_name not in ['datetime', 'tempmin', 'tempmax', 'temp']}

mapping_transformers.update(category_cols)
# Transforming
feature_view = fs.create_feature_view(
    name = 'weather_fv',
    version = 1,
    transformation_functions = mapping_transformers,
    query = query
)
!pip install scikit-learn
import pandas as pd
import sklearn
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import f1_score

import warnings
warnings.filterwarnings("ignore")
feature_view = fs.get_feature_view(
    name = 'weather_fv',
    version = 1
)
feature_view.create_training_data()

train_data = feature_view.get_training_data(1)[0]

train_data.head()
train_data = train_data.sort_values(by=["datetime", 'name'], ascending=[False, True]).reset_index(drop=True)
train_data["temp_next_day"] = train_data.groupby('name')['temp'].shift(1)
train_data["temp_max_next_day"] = train_data.groupby('name')['tempmax'].shift(1)
train_data["temp_min_next_day"] = train_data.groupby('name')['tempmin'].shift(1)

train_data.head(5)

X = train_data.drop(columns=["datetime"]).fillna(0)
y1 = X.pop("temp_next_day")
y2 = X.pop("temp_max_next_day")
y3 = X.pop("temp_min_next_day")

gb1 = GradientBoostingRegressor()
gb2 = GradientBoostingRegressor()
gb3 = GradientBoostingRegressor()
gb1.fit(X, y1)
gb2.fit(X, y2)
gb3.fit(X, y3)

f1_1 = f1_score(y1.astype('int'),[int(pred) for pred in gb1.predict(X)],average='micro')
f1_2 = f1_score(y2.astype('int'),[int(pred) for pred in gb2.predict(X)],average='micro')

f1_3 = f1_score(y3.astype('int'),[int(pred) for pred in gb3.predict(X)],average='micro')

print(y1.iloc[4:10].values)
print(y2.iloc[4:10].values)
print(y3.iloc[4:10].values)
pred_df1 = pd.DataFrame({
    'temp_real': y1.iloc[4:10].values,
    'temp_pred': map(float, gb1.predict(X.iloc[4:10]))
}
)
pred_df1
pred_df2 = pd.DataFrame({
    'temp_max_real': y2.iloc[4:10].values,
    'temp_max_pred': map(float, gb2.predict(X.iloc[4:10]))
}
)
pred_df2
pred_df3 = pd.DataFrame({
    'temp_min_real': y3.iloc[4:10].values,
    'temp_min_pred': map(float, gb3.predict(X.iloc[4:10]))
}
)
pred_df3

mr = project.get_model_registry()

from hsml.schema import Schema
from hsml.model_schema import ModelSchema

input_schema = Schema(X)
output_schema = Schema(y1)
model_schema = ModelSchema(input_schema=input_schema, output_schema=output_schema)

model_schema.to_dict()
import joblib

joblib.dump(gb1, 'model_temp.pkl')
model = mr.sklearn.create_model(
    name="temp_model",
    metrics={"f1": f1_1},
    description="Gradient Boost Regressor.",
    input_example=X.sample().to_numpy(),
    model_schema=model_schema
)

model.save('model_temp.pkl')

from hsml.schema import Schema
from hsml.model_schema import ModelSchema

input_schema = Schema(X)
output_schema = Schema(y2)
model_schema = ModelSchema(input_schema=input_schema, output_schema=output_schema)

model_schema.to_dict()
import joblib

joblib.dump(gb2, 'model_tempmax.pkl')
model = mr.sklearn.create_model(
    name="tempmax_model",
    metrics={"f1": f1_2},
    description="Gradient Boost Regressor.",
    input_example=X.sample().to_numpy(),
    model_schema=model_schema
)

model.save('model_tempmax.pkl')

from hsml.schema import Schema
from hsml.model_schema import ModelSchema

input_schema = Schema(X)
output_schema = Schema(y3)
model_schema = ModelSchema(input_schema=input_schema, output_schema=output_schema)

model_schema.to_dict()
import joblib

joblib.dump(gb3, 'model_tempmin.pkl')
model = mr.sklearn.create_model(
    name="tempmin_model",
    metrics={"f1": f1_3},
    description="Gradient Boost Regressor.",
    input_example=X.sample().to_numpy(),
    model_schema=model_schema
)

model.save('model_tempmin.pkl')
