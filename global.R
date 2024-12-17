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

source("processing/air_quality_processing.R")

# if data for today doesn't exist load air quality data
load_air_quality <- function(date) {
    file_path <- glue("data/air_quality/{date}/active_pm25_london_sensors_{date}.csv")
    if (!file.exists(file_path)) {
        get_active_london_sensors()
        get_individual_sensors()
        aggregate_sensors()
    }
    read_csv(file_path)
}

normalize_raster <- function(r) {
    (r - cellStats(r, "min")) / (cellStats(r, "max") - cellStats(r, "min"))
}

str_date <- Sys.Date()


# LOADING AIR QUALITY DATA:
air_quality <- load_air_quality(str_date) ## UNCOMMENT TO USE RECENTLY UPDATED ('LIVE') DATA
# air_quality <- read_csv("data/air_quality/2024-12-16/active_pm25_london_sensors_2024-12-17.csv") ## UNCOMMENT TO USE STATIC DATA

# convert air quality to a sf object and transform from Global CRS to UK CRS
air_quality_sf <- st_as_sf(air_quality, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = 27700)

# define london boundary from geojson
london_boundary <- read_sf("https://raw.githubusercontent.com/radoi90/housequest-data/refs/heads/master/london_boroughs.geojson") %>%
    st_transform(crs = 27700)
london_bbox <- st_bbox(london_boundary)

# create grid for interpolation
grid <- raster(extent(london_bbox), resolution = 125, crs = st_crs(london_boundary)$proj4string)

#IDW interpolation for air quality
idw_model <- gstat(formula = value ~ 1, locations = air_quality_sf, set = list(idp = 2))
air_quality_raster <- mask(interpolate(grid, idw_model), london_boundary)
air_quality_raster_unnormalised <- mask(interpolate(grid, idw_model), london_boundary)
air_quality_raster <- normalize_raster(air_quality_raster)

# read greenspace shape file, save as sf object, transform to UK CRS
greenspace <- st_read("data/GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp") %>%
    st_transform(crs = 27700)

greenspace_raster <- rasterize(st_as_sf(greenspace), air_quality_raster, field = 1, background = NA) #rasterise green space
greenspace_raster <- distance(greenspace_raster) # compute green space distance
greenspace_raster <- exp(-greenspace_raster / 2000) # decay constant to determine importance of proximity
greenspace_raster <- normalize_raster(1 - greenspace_raster)

# default wellbeing metric raster
wellbeing_raster <- normalize_raster(1 - (0.5 * air_quality_raster + 0.5 * greenspace_raster))

#only for greenspace visualization layer
greenspace <- st_read("data/GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp")
greenspace <- st_transform(greenspace, crs = 4326)