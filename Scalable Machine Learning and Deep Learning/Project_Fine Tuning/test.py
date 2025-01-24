import hopsworks
project = hopsworks.login()

fs = project.get_feature_store() 

weather_fg = fs.get_or_create_feature_group(
        name = 'hel_air_quality_fg',
        description = 'Weather characteristics of each day',
        version = 1
    )

weather_fg.delete()

weather_fg = fs.get_or_create_feature_group(
        name = 'hel_weather_fg',
        description = 'Weather characteristics of each day',
        version = 1
    )

weather_fg.delete()