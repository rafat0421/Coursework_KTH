import gradio as gr
import hopsworks
import joblib
import pandas as pd
import numpy as np
import json
import time
from datetime import timedelta, datetime


from functions import *


project = hopsworks.login()
fs = project.get_feature_store()
	

def air_quality(city):
    weather_df = pd.DataFrame()
    for i in range(8):
        weather_data = get_weather_df([get_weather_data((datetime.now() + timedelta(days=i)).strftime("%Y-%m-%d"))])
        weather_df = weather_df.append(weather_data)
        
    weather_df = weather_df.drop(columns=["feelslikemin", "feelslikemax", "precipprob", "snowdepth", "uvindex", "date","city","conditions"]).fillna(0)
	    
    mr = project.get_model_registry()
    model = mr.get_model("hel_gradient_boost_model", version=1)
    model_dir = model.download()
    model = joblib.load(model_dir + "/hel_model.pkl")
	    
    preds = model.predict(weather_df)
	
    predictions = ''
    for k in range(7):
        predictions += "Predicted AQI on  " + (datetime.now() + timedelta(days=k)).strftime('%Y-%m-%d') + ":      " + str(int(preds[k]))+"\n"
	        
        print(predictions)
    return predictions
	
    
demo = gr.Interface(fn=air_quality, title="Air quality predictor",
description="Input a value to get next weeks AQI prediction for Helsinki", inputs="text", outputs="text")
	

	    
if __name__ == "__main__":
	demo.launch()
