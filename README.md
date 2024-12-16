
## Setup

### Packages
To install all required packages first run requirements.R

### OpenAQ Setup
Please create a .Renviron file in your local directory which will be used to store your API key in the form:

```
OPEN_AQ_API_KEY = [YOUR API KEY]
```

To create API Key please sign-up at OpenAQ: https://docs.openaq.org/using-the-api/api-key

## Running App
Once all set-up steps have been completed the Shiny app should be possible to run using:

```
shiny::runApp("app.R")
```

Please note if this is the first time you are running the app today it may be quite slow as it first needs to collect all of the sensor data for today 

### Using template air quality data
For optimal air quality data the daily data collection process can be skipped and the sample air quality file can be used instead, which will have the best volume of information - but is not acurate to your specific day 
