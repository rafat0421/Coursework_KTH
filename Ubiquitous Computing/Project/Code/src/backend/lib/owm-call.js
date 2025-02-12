const fetch = require("node-fetch");
const config = {
  apikey: "df85738b893855278d6f48eb35250501",
  lang: "en",
  city: "Stockholm",
  unit: "metric",
  test_id: 3384868,
};

const hotLocation = false;

const URL = getURL();

//Get current weather data
function getWeatherData() {
  console.log("Getting weather data from: " + URL);

  return fetch(URL)
    .then(function (response) {
      return response.json();
    })
    .then(function (data) {
      var temperature = Math.ceiling(
        parseFloat((data.main.temp + data.main.feels_like) / 2)
      );
      return {
        id: data.weather[0].id,
        temperature,
        description: data.weather[0].description,
        city: data.name,
        sunrise: data.sys.sunrise,
        sunset: data.sys.sunset,
        lat: data.coord.lat,
        lon: data.coord.lon,
      };
    });
}

//Get alerts of the current location
function getAlerts(lat, lon) {
  let alertUrl =
    "https://api.openweathermap.org/data/2.5/onecall?&lat=" +
    lat +
    "&lon=" +
    lon +
    "&units=" +
    config.unit +
    "&lang=" +
    config.lang +
    "&appid=" +
    config.apikey +
    "&exclude=minutely,hourly,daily";
  console.log("Getting alert from: " + alertUrl);
  return fetch(alertUrl)
    .then(function (response) {
      return response.json();
    })
    .then(function (data) {
      try {
        var alertData = {
          sender_name: data.alerts[0].sender_name,
          event: data.alerts[0].event,
          start: data.alerts[0].start,
          end: data.alerts[0].end,
          description: data.alerts[0].description,
        };
      } catch (e) {
        console.log("No alert found.");
        return;
      }
      console.log(alertData);
      return alertData;
    });
}

//Assembles API URL based on hot/cold location
function getURL() {
  let url = "http://api.openweathermap.org/data/2.5/weather?";
  if (hotLocation) {
    //Get data for hot location (Brazil)
    url += "id=" + config.test_id;
  } else {
    //Get data for cold location (Stockholm)
    url += "q=" + config.city;
  }
  return (
    url +
    "&units=" +
    config.unit +
    "&lang=" +
    config.lang +
    "&appid=" +
    config.apikey
  );
}
module.exports = { getWeatherData, getAlerts };
