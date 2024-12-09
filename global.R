library(shiny)
library(leaflet)
library(glue)
library(sp)
library(gstat)
library(sf)
library(ggplot2)
library(plotly)
library(readr)
library(data.table)
library(httr)
library(jsonlite)
library(raster)
library(dplyr)
library(stringr)

api_key <- Sys.getenv("OPEN_AQ_API_KEY")

# Fetch live air quality data
get_london_air_quality <- function() {
    res <- GET(
        "https://api.openaq.org/v3/locations?coordinates=51.508045,-0.128217&radius=25000&limit=1000",
        add_headers(`X-API-Key` = api_key)
    )
    openaq_data <- fromJSON(rawToChar(res$content))
    results <- as.data.frame(openaq_data$results)

    cords <- results$coordinates
    latest <- results$datetimeLast
    df <- data.frame(results$id, results$name, cords$latitude, cords$longitude, latest$local)
    colnames(df) <- c("id", "name", "latitude", "longitude", "latest")

    write.csv(df, "data/air_quality/london_sensor_data_full.csv")
}

filter_latest_air_quality <- function(file_path, str_date) {
    sensor_data <- read.csv(file_path)
    current_sensor_data <- filter(sensor_data, str_sub(latest, 0, 10) == str_date)
    write.csv(current_sensor_data, glue("data/air_quality/london_sensor_data_{str_date}.csv"))
}

get_sensor_values <- function(file_path, str_date) {
    sensors <- read.csv(file_path)
    ids <- sensors$id

    li <- list()
    for (element in ids) {
        res <- GET(
            glue("https://api.openaq.org/v3/locations/{element}/latest"),
            add_headers(`X-API-Key` = api_key)
        )
        openaq_data <- fromJSON(rawToChar(res$content))
        results_data <- as.data.frame(openaq_data$results)
        li <- append(li, results_data$value[1])
    }
    sensors$value <- c(li)
    sensors <- apply(sensors, 2, as.character)
    write.csv(sensors, glue("data/air_quality/london_sensor_data_values_{str_date}.csv"))
}

# Current date
date <- Sys.Date()
str_date <- toString(date)

file_path <- glue("data/air_quality/london_sensor_data_values_{str_date}.csv")
if (file.exists(file_path)) {
    air_quality <- read_csv(glue("data/air_quality/london_sensor_data_values_{str_date}.csv"))
} else {
    get_london_air_quality()
    filter_latest_air_quality("data/air_quality/london_sensor_data_full.csv", str_date)
    get_sensor_values(glue("data/air_quality/london_sensor_data_{str_date}.csv"), str_date)
    air_quality <- read_csv(glue("data/air_quality/london_sensor_data_values_{str_date}.csv"))
}

# Greenspace shapefile
greenspace <- st_read("data/GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp")
greenspace <- st_transform(greenspace, crs = 4326)
greenspace_for_raster <- st_transform(greenspace, crs = 27700)

air_quality <- read_csv("data/air_quality/london_sensor_data_values_2024-12-08.csv")
air_quality <- filter(air_quality, name != "Twickenham")

# Spatial object
air_quality_sf <- st_as_sf(
    air_quality,
    coords = c("longitude", "latitude"),
    crs = 4326
)
air_quality_sf <- st_transform(air_quality_sf, crs = 27700)

air_quality_sp <- sp::SpatialPointsDataFrame(
    coords = st_coordinates(air_quality_sf),
    data = as.data.frame(air_quality_sf),
    proj4string = sp::CRS(as.character(st_crs(air_quality_sf)$proj4string))
)

# Interpolation grid
london_bbox <- bbox(air_quality_sp)
grid <- raster(
    extent(london_bbox),
    resolution = 125,
    crs = sp::CRS("EPSG:27700")
)

# IDW interpolation
idw_model <- gstat(
    formula = value ~ 1,
    locations = air_quality_sp,
    set = list(idp = 2)
)
air_quality_raster <- interpolate(grid, idw_model)

# Normalize raster values 
air_quality_raster <- (air_quality_raster - cellStats(air_quality_raster, "min")) /
    (cellStats(air_quality_raster, "max") - cellStats(air_quality_raster, "min"))

# Greenspace rasterization
greenspace_raster <- rasterize(
    x = st_as_sf(greenspace_for_raster),
    y = air_quality_raster,
    field = 1,
    background = NA
)

#make it so that the greenspace part in wellbeing metric measures greenspace proximity
greenspace_raster <- distance(greenspace_raster)

decay_constant <- 2000  # constant for importance of proximity 
greenspace_raster <- exp(-greenspace_raster / decay_constant)

greenspace_raster <- (greenspace_raster - cellStats(greenspace_raster, "min")) /
                    (cellStats(greenspace_raster, "max") - cellStats(greenspace_raster, "min"))
greenspace_raster <- 1 - greenspace_raster

#default wellbeing raster
air_weight <- 0.5
green_weight <- 0.5
wellbeing_raster <- air_weight * air_quality_raster +
    green_weight * greenspace_raster
wellbeing_raster <- (wellbeing_raster - cellStats(wellbeing_raster, "min")) /
    (cellStats(wellbeing_raster, "max") - cellStats(wellbeing_raster, "min"))
# Invert the wellbeing raster
wellbeing_raster <- 1 - wellbeing_raster