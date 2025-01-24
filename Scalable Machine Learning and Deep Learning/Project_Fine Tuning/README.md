# Project (air-quality)
As a measure of air pollution, the air quality index (AQI) describes the pollution level in a particular area. Particulate matter, ozone, carbon monoxide, and nitrogen dioxide are measured as pollutants in the air. AQI indicates how polluted the air is.

As part of this project, we used historical air quality and weather data in Helsinki to train our model to predict AQI in the future. This project aims to predict the AQI in Helsinki for the next seven days.
# Data Source

## Air Quality
Data for air quality was gathered from the World Air Quality Project.

https://aqicn.org//here/

## Weather Conditions
Data for weather conditions was gathered from Visual Crossing.

https://www.visualcrossing.com/

# Project Architecture
Required Credentials:
1. Hposworks API-key: Stored in the .hw_api_key configuration file.
2. AQI and Weather API-key: Stored in the .env configuration file.

The project include the following pipelines:

1. Use 1_backfill_feature_groups.py to parse the Helsinki's historical air-quality and weather data (from csv file) and create feature groups.
2. Use 2_feature_pipeline.py to create/modify feature groups and update daily data.
3. Use 3_feature_views_and_training_dataset.py to combine two feature groups into a feature view and generate training pipline.
4. Use 4_model_training.py to train the model.

Project architecture diagram:

[Project Architecture](images/ID2223-Project.png)

#User Interface

