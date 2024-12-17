
## Setup

### Packages
To install all required packages first run requirements.R

### OpenAQ Setup
Please create a .Renviron file in your local directory which will be used to store your API key in the form:

```
OPEN_AQ_API_KEY = [YOUR API KEY]
```

To create API Key please sign-up at OpenAQ: https://docs.openaq.org/using-the-api/api-key
This is required to display recently collected air quality data - if you would like to skip this step please comment out this line in `global.R`:
```
air_quality <- load_air_quality(str_date)
```

and uncomment this line to use a previously stored data-frame instead:
```
air_quality <- read_csv("data/air_quality/2024-12-16/active_pm25_london_sensors_2024-12-17.csv")
```

**Additional Notes:** There are several limitations in the 'live' data collection abilities of the app, which may include issues associated with people using the platform in different time zones (other than the UK), or if it is very close to 12AM (e.g. 1AM-3AM) since there might not have been enough time to collect enough data between these hours - if any problems occur related to this please follow the steps above to use static air quality data instead.

## Running App
Once all set-up steps have been completed the Shiny app should be possible to run using:

```
shiny::runApp("app.R")
```

Please note if this is the first time you are running the app today it may be quite slow as it first needs to collect all of the sensor data for today 

### Using template air quality data
For optimal air quality data the daily data collection process can be skipped and the sample air quality file can be used instead, which will have the best volume of information - but is not acurate to your specific day 
