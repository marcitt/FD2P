# global.R: Data preprocessing and raster preparation

# Load required libraries
library(shiny)
library(leaflet)
library(glue)
library(sp)
library(gstat)
library(sf)
library(readr)
library(raster)
library(dplyr)
library(httr)
library(jsonlite)

# Load helper functions for air quality
source("processing/air_quality_processing.R")

# Helper: Fetch or load processed air quality data
load_air_quality <- function(date) {
    file_path <- glue("data/air_quality/{date}/active_pm25_london_sensors_{date}.csv")
    if (!file.exists(file_path)) {
        get_active_london_sensors()
        get_individual_sensors()
        aggregate_sensors()
    }
    read_csv(file_path)
}

# Helper: Normalize a raster layer
normalize_raster <- function(r) {
    (r - cellStats(r, "min")) / (cellStats(r, "max") - cellStats(r, "min"))
}

# Current date for processing
str_date <- Sys.Date()

# 1. Load air quality data
air_quality <- load_air_quality(str_date)
air_quality_sf <- st_as_sf(air_quality, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = 27700)

# 2. Create spatial grid for interpolation
london_boundary <- read_sf("https://raw.githubusercontent.com/radoi90/housequest-data/refs/heads/master/london_boroughs.geojson") %>%
    st_transform(crs = 27700)
london_bbox <- st_bbox(london_boundary)
grid <- raster(extent(london_bbox), resolution = 125, crs = st_crs(london_boundary)$proj4string)

# 3. IDW interpolation for air quality
idw_model <- gstat(formula = value ~ 1, locations = air_quality_sf, set = list(idp = 2))
air_quality_raster <- mask(interpolate(grid, idw_model), london_boundary)
air_quality_raster <- normalize_raster(air_quality_raster)

# 4. Greenspace proximity calculation
greenspace <- st_read("data/GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp") %>%
    st_transform(crs = 27700)

greenspace_raster <- rasterize(st_as_sf(greenspace), air_quality_raster, field = 1, background = NA)
greenspace_raster <- distance(greenspace_raster)
greenspace_raster <- exp(-greenspace_raster / 2000) # Decay constant for proximity importance
greenspace_raster <- normalize_raster(1 - greenspace_raster)

# 5. Default wellbeing metric raster
wellbeing_raster <- normalize_raster(1 - (0.5 * air_quality_raster + 0.5 * greenspace_raster))