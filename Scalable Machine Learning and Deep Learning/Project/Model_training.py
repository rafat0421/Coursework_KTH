import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import f1_score
import warnings
import hopsworks
from hsml.schema import Schema
from hsml.model_schema import ModelSchema
import joblib

warnings.filterwarnings("ignore")

project = hopsworks.login() 

fs = project.get_feature_store() 

feature_view = fs.get_feature_view(
    name = 'hel_air_fv1',
    version = 1
)

train_data = feature_view.get_training_data(1)[0]

#train_data.head()

train_data = train_data.sort_values(by=["date", 'city'], ascending=[False, True]).reset_index(drop=True)
train_data["aqi_next_day"] = train_data.groupby('city')['aqi'].shift(1)

#train_data.head(5)

X = train_data.drop(columns=["date"]).fillna(0)
y = X.pop("aqi_next_day")

gb = GradientBoostingRegressor()
gb.fit(X, y)

f1_score(y.astype('int'),[int(pred) for pred in gb.predict(X)],average='micro')

y.iloc[4:10].values

pred_df = pd.DataFrame({
    'aqi_real': y.iloc[4:10].values,
    'aqi_pred': map(int, gb.predict(X.iloc[4:10]))
}
)

#pred_df

mr = project.get_model_registry()

input_schema = Schema(X)
output_schema = Schema(y)
model_schema = ModelSchema(input_schema=input_schema, output_schema=output_schema)

model_schema.to_dict()

joblib.dump(gb, 'model.pkl')

model = mr.sklearn.create_model(
    name="gradient_boost_model",
    metrics={"f1": "0.5"},
    description="Gradient Boost Regressor.",
    input_example=X.sample(),
    model_schema=model_schema
)

model.save('model.pkl')



