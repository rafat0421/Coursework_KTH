import gradio as gr
import hopsworks
import joblib
import pandas as pd
import numpy as np
import folium
import json
import time
from datetime import timedelta, datetime
from branca.element import Figure
from functions import decode_features, get_model, get_model1, getmodel2


def greet(total_pred_days):

    project = hopsworks.login()
    #api = project.get_dataset_api()
    fs = project.get_feature_store()
    feature_view = fs.get_feature_view(
        name = 'weather_fv',
        version = 1
    )

    # The latest available data timestamp
    start_time = 1635112800000

    #start_date = datetime.now() - timedelta(days=1)
    #start_time = int(start_date.timestamp()) * 1000


    X = feature_view.get_batch_data(start_time=start_time)
    latest_date_unix = str(X.datetime.values[0])[:10]
    latest_date = time.ctime(int(latest_date_unix))
    model = get_model(project=project,
                      model_name="temp_model",
                      evaluation_metric="f1_score",
                      sort_metrics_by="max")
    model1 = get_model1(project=project,
                      model_name="tempmax_model",
                      evaluation_metric="f1_score",
                      sort_metrics_by="max") 
    model2 = get_model2(project=project,
                      model_name="tempmin_model",
                      evaluation_metric="f1_score",
                      sort_metrics_by="max")
    X = X.drop(columns=["datetime"]).fillna(0)

    preds = model.predict(X)
    
    preds1= model1.predict(X)
    
    preds2= model2.predict(X)
    

   # cities = [city_tuple[0] for city_tuple in cities_coords.keys()]

    next_day_date = datetime.today() + timedelta(days=1)
    next_day = next_day_date.strftime ('%d/%m/%Y')
    str1 = ""
    
    if(total_pred_days == ""):
        return "Empty input"
    
    count = int(total_pred_days)
    if count > 20:
        str1 += "Warning: 20 days at most. " + '\n'
        count = 20
    if count <0:
        str1 = "Invalid input."
        return str1
    
    for x in range(count):
        if (x != 0):
            str1 += (datetime.now() + timedelta(days=x)).strftime('%Y-%m-%d') + " predicted temperature:      " +str(int(preds[len(preds) - count + x]))+ " predicted max temperature:      " +str(int(preds1[len(preds1) - count + x]))+ " predicted min temperature:      " +str(int(preds2[len(preds2) - count + x]))+"\n"
    
    #print(str1)
    return str1


demo = gr.Interface(fn=greet, inputs = "text", outputs="text")


    
if __name__ == "__main__":
    demo.launch()
