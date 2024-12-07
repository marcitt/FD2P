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

library(shiny)
library(leaflet)
library(sf)
library(readr)
library(gstat)
library(sp)
library(raster)
library(dplyr)

library("stringr")

api_key <- Sys.getenv("OPEN_AQ_API_KEY")

# Load greenspace shapefile
greenspace <- st_read("data/GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp") # Load greenspace shapefile
greenspace <- st_transform(greenspace, crs = 4326) # Reproject to WGS84

air_quality <- read_csv("data/air-quality-2024-12-02.csv")
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
