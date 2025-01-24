# Goal
Air quality index (AQI) is a measure used to describe the level of air pollution in a particular area. It is calculated based on the levels of certain pollutants in the air, such as particulate matter, ozone, carbon monoxide, and nitrogen dioxide. The higher the AQI, the more polluted the air is.

Predicting AQI in Helsinki can be useful for a variety of purposes, such as to help people with respiratory issues plan their activities, to inform decisions about transportation modes, and to support policy makers in developing strategies to reduce air pollution.

This project aims at predicting the AQI in Helsinki for the next 7 days based on previous weather and air conditions.

# Hugging Face url

https://huggingface.co/spaces/Orangefish/project

To see the predictions, press "Submit" with random input.

![Image text](https://raw.githubusercontent.com/Man-bearpig/ID2223/main/project/app.png)

# Data Source

## Air Quality

https://aqicn.org//here/

## Weather Conditions

https://www.visualcrossing.com/

# Project Architecture

The project include the following steps:

1. Request API-key in order to request data from data source, store the API-key in the .env configuration file.
2. Use Backfill_feature_groups.py to download Helsinki's historical weather data from data source and parse the data.
3. Use Feature_pipeline.py to create feature pipelines and update daily data.
4. Use Feature_views_and_training_dataset.py to combine feature piplines into a feature view and generate training pipline.
5. Use Model_training.py to train the model.
6. Use app.py to start a Gradio app and predict AQI in Helsinki in the next 7 days.
