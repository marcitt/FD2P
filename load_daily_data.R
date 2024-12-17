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

# Current date for processing
str_date <- Sys.Date()

# 1. Load air quality data
air_quality <- load_air_quality(str_date)