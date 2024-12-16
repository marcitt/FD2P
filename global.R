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

source("processing/air_quality_processing.R")

str_date <- Sys.Date()

file_path <- glue("data/air_quality/{str_date}/active_pm25_london_sensors_{str_date}.csv")

# Check if air quality data exists
if (file.exists(file_path)) {
    air_quality <- read_csv(file_path)
} else {
    get_active_london_sensors()
    get_individual_sensors()
    aggregate_sensors()
    air_quality <- read_csv(file_path)
}

# Convert to sf spatial object
air_quality_sf <- st_as_sf(
    air_quality,
    coords = c("longitude", "latitude"),
    crs = 4326
)
air_quality_sf <- st_transform(air_quality_sf, crs = 27700)

london_boundary <- read_sf("https://raw.githubusercontent.com/radoi90/housequest-data/refs/heads/master/london_boroughs.geojson")
london_boundary <- st_transform(london_boundary, crs = 27700)

london_bbox <- st_bbox(london_boundary) # Get the bounding box (extent) of London's polygon
grid <- raster(
    extent(london_bbox), # Use the bounding box extent
    resolution = 125, # Define grid resolution
    crs = st_crs(london_boundary)$proj4string # Use CRS of the London boundary shapefile
)

# IDW interpolation directly with sf object
idw_model <- gstat(
    formula = value ~ 1,
    locations = air_quality_sf, # Using sf object directly
    set = list(idp = 2) # Inverse Distance Weighting (IDW)
)
air_quality_raster <- interpolate(grid, idw_model)
air_quality_raster <- mask(air_quality_raster, london_boundary)

# Normalize raster values
air_quality_raster <- (air_quality_raster - cellStats(air_quality_raster, "min")) /
    (cellStats(air_quality_raster, "max") - cellStats(air_quality_raster, "min"))

# Greenspace shapefile
greenspace <- st_read("data/GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp")
greenspace <- st_transform(greenspace, crs = 4326)
greenspace_for_raster <- st_transform(greenspace, crs = 27700)

# Greenspace rasterization
greenspace_raster <- rasterize(
    x = st_as_sf(greenspace_for_raster),
    y = air_quality_raster,
    field = 1,
    background = NA
)

# make it so that the greenspace part in wellbeing metric measures greenspace proximity
greenspace_raster <- distance(greenspace_raster)

decay_constant <- 2000 # constant chosen for importance of proximity in the wellbeing metric
greenspace_raster <- exp(-greenspace_raster / decay_constant)

greenspace_raster <- (greenspace_raster - cellStats(greenspace_raster, "min")) /
    (cellStats(greenspace_raster, "max") - cellStats(greenspace_raster, "min"))
greenspace_raster <- 1 - greenspace_raster

# default wellbeing raster
air_weight <- 0.5
green_weight <- 0.5
wellbeing_raster <- air_weight * air_quality_raster +
    green_weight * greenspace_raster
wellbeing_raster <- (wellbeing_raster - cellStats(wellbeing_raster, "min")) /
    (cellStats(wellbeing_raster, "max") - cellStats(wellbeing_raster, "min"))
# Invert the wellbeing raster
wellbeing_raster <- 1 - wellbeing_raster