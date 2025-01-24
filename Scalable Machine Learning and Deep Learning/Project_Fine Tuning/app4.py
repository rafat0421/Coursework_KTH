import streamlit as st
import hopsworks
import joblib
import pandas as pd
from datetime import *
from functions import *
import pytz
#from datetime import timedelta

st.set_page_config(layout="wide")

st.title('AQI prediction for Helsinki for the next week')

project = hopsworks.login()

today=datetime.now()

weather_df = pd.DataFrame()
for i in range(8):
        weather_data = get_weather_df([get_weather_data("Helsinki", (datetime.now() + timedelta(days=i)).strftime("%Y-%m-%d"))])
        weather_df = weather_df.append(weather_data)

# get Hopsworks Model Registry
mr = project.get_model_registry()
# get model object
model = mr.get_model("hel_gradient_boost_model", version=1)
model_dir = model.download()
model = joblib.load(model_dir + "/hel_model.pkl")
fs = project.get_feature_store()
feature_view = fs.get_feature_view(
    name = 'hel_air_quality_fv',#helsinki_aqi_fv
    version = 1
)
start_date = datetime.now() - timedelta(days=1)
start_time = int(start_date.timestamp()) * 1000
X = feature_view.get_batch_data(start_time=start_time)

# MAP

cities_coords = {("helsinki", "finland"): [60.1699, 24.9384]}

city = "helsinki"
country = "finland"
text = f"""
            <h4 style="color:green;">{city}</h4>
            <h5 style="color":"green">
                <table style="text-align: right;">
                    <tr>
                        <th>Country:</th>
                        <td><b>{country}</b></td>
                    </tr>"""




# Other

weather_df = weather_df.drop(columns=["feelslikemin", "feelslikemax", "precipprob", "snowdepth", "uvindex", "date","city","conditions"]).fillna(0)


temp = weather_df['temp'].to_numpy()
humidity = weather_df['humidity'].to_numpy()
visibility = weather_df['visibility'].to_numpy()
precip = weather_df['precip'].to_numpy()
dew = weather_df['dew'].to_numpy()
pressure = weather_df['pressure'].to_numpy()

preds=model.predict(weather_df)

next_week = [f"{(today + timedelta(days=d)).strftime('%A')} {(today + timedelta(days=d)).strftime('%Y-%m-%d')}" for d in range(8)]
#df = pd.DataFrame(data=[weekly_data['temp'], weekly_data['precip']], index=["temp","precip"], columns=next_week)
#st.write(weekly_data['temp'])

# Data prep
aqi_level = encoder_range(preds.T.reshape(-1, 1))
#cols_= ["temp", "humidity", "visibility", "precip", "cloudcover", "uvindex"]
#weekly_data[cols_] = weekly_data[cols_].apply(lambda x: round(x, 1))

df = pd.DataFrame(data=[temp, humidity, visibility, precip, dew, pressure,map(int,preds), aqi_level],
                  index=["Temperature", "Humidity", "Visibility", "Precipitation", "Dew", "Pressure",
                         "Air Quality Index","Air Pollution Level"], columns=next_week)

st.write(df)

st.button("Re-run")