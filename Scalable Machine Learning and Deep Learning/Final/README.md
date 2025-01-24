# Goal

Temperature is an indispensable and important weather factor in daily life. It may affect people's daily travel, going out plans and some precise calculations. Therefore, the prediction of temperature is very important.

Predicting the temperature in Shanghai can be used for a variety of purposes, such as helping the government formulate policies that are more in line with sustainable development goals, assisting in the development of transportation modes, and helping people plan their daily travel.

This project aims to predict the temperature in Shanghai in the next few days (less than or equal to 20 days) based on the previous weather conditions, including the highest temperature, the lowest temperature and the average temperature.

# Hugging Face url

https://huggingface.co/spaces/Campfireman/temperature_pred

You need to enter the number of days you want to predict and press "Submit".

<img width="960" alt="image" src="https://user-images.githubusercontent.com/89634392/210854651-533f1521-ca17-4080-8f52-c6dd3fc6c262.png">

# Data source

https://www.visualcrossing.com/weather/weather-data-services


# Project Architecture

The project include the following steps:

  1. Request API-key in order to request data from data source, store the API-key in the .env configuration file.
  2. Use feature_pipeline.py to download Shanghai's historical weather data from data source and parse the data.
  4. Use training_pipeline.py to combine feature piplines into a feature view and train the model.
  6. Use app.py to start a Gradio app and predict temperature in Shanghai in the next few days.


