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

from functions import decode_features, get_model

def greet(name):

    project = hopsworks.login()
    api = project.get_dataset_api()
    fs = project.get_feature_store()
    feature_view = fs.get_feature_view(
        name = 'hel_air_fv1',
        version = 1
    )

    start_time = 1671058800000
    #start_date = datetime.now() - timedelta(days=1)
    #start_time = int(start_date.timestamp()) * 1000


    X = feature_view.get_batch_data(start_time=start_time)
    latest_date_unix = str(X.date.values[0])[:10]
    latest_date = time.ctime(int(latest_date_unix))

    X = X.drop(columns=["date"]).fillna(0)


    model = get_model(project=project,
                      model_name="gradient_boost_model",
                      evaluation_metric="f1_score",
                      sort_metrics_by="max")

    preds = model.predict(X)

   # cities = [city_tuple[0] for city_tuple in cities_coords.keys()]

    next_day_date = datetime.today() + timedelta(days=1)
    next_day = next_day_date.strftime ('%d/%m/%Y')
 #   df = pd.DataFrame(data=preds[0], columns=[f"AQI Predictions for {next_day}"], dtype=int)
    str1 = ""
#    return int(preds[0])


    for x in range(8):
      if(x != 0):
         str1 += (datetime.now() + timedelta(days=x)).strftime('%Y-%m-%d') + " predicted aqi:      " + str(int(preds[len(preds) - 8 + x]))+"\n"
    
    print(str1)
    return str1


demo = gr.Interface(fn=greet, inputs="text", outputs="text")


    
if __name__ == "__main__":
    demo.launch()
