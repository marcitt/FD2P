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


library("stringr")

api_key <- Sys.getenv("OPEN_AQ_API_KEY")

# # LIVE AIR QUALITY
# get_london_air_quality <- function() {
#     res <- GET(
#         # get all sensors within a radius of 25000
#         "https://api.openaq.org/v3/locations?coordinates=51.508045,-0.128217&radius=25000&limit=1000",
#         add_headers(`X-API-Key` = api_key)
#     )

#     openaq_data <- fromJSON(rawToChar(res$content))
#     results <- as.data.frame(openaq_data$results)

#     cords <- results$coordinates
#     latest <- results$datetimeLast
#     df <- data.frame(results$id, results$name, cords$latitude, cords$longitude, latest$local)
#     colnames(df) <- c("id", "name", "latitude", "longitude", "latest")

#     write.csv(df, "data/air_quality/london_sensor_data_full.csv")
# }

# filter_latest_air_quality <- function(file_path, str_date) {
#     sensor_data <- read.csv(file_path)
#     current_sensor_data <- filter(sensor_data, str_sub(latest, 0, 10) == str_date)
#     write.csv(current_sensor_data, glue("data/air_quality/london_sensor_data_{str_date}.csv"))
# }

# get_sensor_values <- function(file_path, str_date) {
#     sensors <- read.csv(file_path)
#     ids <- sensors$id

#     li <- list()

#     # iterate through ids and get first sensor reading
#     for (element in ids) {
#         res <- GET(
#             glue("https://api.openaq.org/v3/locations/{element}/latest"),
#             add_headers(`X-API-Key` = api_key)
#         )
#         openaq_data <- fromJSON(rawToChar(res$content))
#         results_data <- as.data.frame(openaq_data$results)
#         li <- append(li, results_data$value[1]) # first sensor reading in list
#     }

#     # add new column for sensor values
#     sensors$value <- c(li)

#     # https://stackoverflow.com/questions/24829027/unimplemented-type-list-when-trying-to-write-table
#     sensors <- apply(sensors, 2, as.character)
#     write.csv(sensors, glue("data/air_quality/london_sensor_data_values_{str_date}.csv"))
# }

# # get current date
# date <- Sys.Date()
# str_date <- toString(date)

# # check if sensor values have been computed for this day: 
# file_path <- glue("data/air_quality/london_sensor_data_values_{str_date}.csv")
# if (file.exists(file_path)) {
#    air_quality <- read_csv(glue("data/air_quality/london_sensor_data_values_{str_date}.csv"))
# } else {
#     get_london_air_quality()
#     filter_latest_air_quality("data/air_quality/london_sensor_data_full.csv", str_date)
#     get_sensor_values(glue("data/air_quality/london_sensor_data_{str_date}.csv"), str_date)
#     air_quality <- read_csv(glue("data/air_quality/london_sensor_data_values_{str_date}.csv"))
# }

air_quality <- read.csv("data/air_quality/london-filtered-sensors-2024-12-14.csv")

# Load greenspace shapefile
greenspace <- st_read("data/GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp") # Load greenspace shapefile
greenspace <- st_transform(greenspace, crs = 4326) # Reproject to WGS84

air_quality <- filter(air_quality, name != "Twickenham")

# spatial object
air_quality_sf <- st_as_sf(
    air_quality,
    coords = c("longitude", "latitude"),
    crs = 4326 # WGS84
)

# Reproject air quality data to British National Grid
air_quality_sf <- st_transform(air_quality_sf, crs = 27700)

# Convert air quality data to SpatialPointsDataFrame for interpolation
air_quality_sp <- sp::SpatialPointsDataFrame(
    coords = st_coordinates(air_quality_sf),
    data = as.data.frame(air_quality_sf),
    proj4string = sp::CRS(as.character(st_crs(air_quality_sf)$proj4string))
)

# Create interpolation grid
london_bbox <- bbox(air_quality_sp)
grid <- raster(
    extent(london_bbox),
    resolution = 125, # Set grid cell size
    crs = sp::CRS("+init=epsg:27700") # British National Grid
)

# Perform IDW interpolation
idw_model <- gstat(
    formula = value ~ 1,
    locations = air_quality_sp,
    set = list(idp = 2) # Power parameter
)

# Interpolate the grid
air_quality_raster <- interpolate(grid, idw_model)

# Normalize raster values
air_quality_raster <- (air_quality_raster - cellStats(air_quality_raster, "min")) /
    (cellStats(air_quality_raster, "max") - cellStats(air_quality_raster, "min"))
