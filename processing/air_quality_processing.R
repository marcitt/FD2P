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

# General function to get a response from an endpoint
get_endpoint <- function(endpoint) {
    res <- GET(
        glue(endpoint),
        add_headers(`X-API-Key` = api_key)
    )

    parsed <- fromJSON(rawToChar(res$content))
    return(parsed$results)
}

get_active_london_sensors <- function() {
    # get all sensors for London
    london_sensors <- get_endpoint("https://api.openaq.org/v3/locations?coordinates=51.508045,-0.128217&radius=25000&limit=1000")

    str_date <- Sys.Date()

    # create file structure for subsequent data
    dir_path <- glue("data/air_quality/{str_date}")
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
    }

    dir_path <- glue("data/air_quality/{str_date}/location_data")
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
    }

    # filter for recently active sensors (have recorded at least one reading for today )
    active_london_sensors <- filter(london_sensors, str_sub(datetimeLast$utc, 0, 10) == str_date)
    reduced_active_london_sensors <- select(active_london_sensors, -country, -bounds, -owner, -provider, -isMobile, -isMonitor, -instruments, -sensors, -coordinates, -licenses, -datetimeFirst, -datetimeLast)
    write.csv(reduced_active_london_sensors, glue("data/air_quality/{str_date}/active_london_sensors_{str_date}.csv"))
}

record_location_df <- function(location_id) {
    res <- GET(
        glue("https://api.openaq.org/v3/locations/{location_id}/sensors"),
        add_headers(`X-API-Key` = api_key)
    )
    parsed <- fromJSON(rawToChar(res$content))

    # Check if 'results' exist
    if (is.null(parsed$results) || length(parsed$results) == 0) {
        message("No data found for location: ", location_id)
        return(NULL) # Skip this location
    }

    df <- parsed$results

    # Ensure 'latest' exists
    if (is.null(df$latest)) {
        message("No 'latest' data for location: ", location_id)
        return(NULL) # Skip this location
    }

    latest_date <- df$datetimeLast
    utc <- latest_date$utc

    # Ensure 'coordinates' exist
    if (is.null(df$latest$coordinates)) {
        message("No coordinates for location: ", location_id)
        return(NULL) # Skip this location
    }

    coordinates <- df$latest$coordinates

    df$latestDateUTC <- unlist(utc)
    df$value <- unlist(df$latest$value)
    df$latitude <- unlist(coordinates$latitude)
    df$longitude <- unlist(coordinates$longitude)

    # Select relevant columns (avoid errors if they don't exist)
    df <- df %>%
        select(-datetimeFirst, -datetimeLast, -parameter, -coverage, -summary, -latest)

    str_date <- Sys.Date()

    # Check if directory exists, create if not
    dir_path <- glue("data/air_quality/{str_date}/location_data")
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
    }


    write.csv(df, glue("data/air_quality/{str_date}/location_data/location_{location_id}_{str_date}.csv"))
}

get_individual_sensors <- function() {
    str_date <- Sys.Date()
    active_london_sensors <- read.csv(glue("data/air_quality/{str_date}/active_london_sensors_{str_date}.csv"))

    location_ids <- active_london_sensors$id

    for (location in location_ids) {
        record_location_df(location)
    }
}

aggregate_sensors <- function() {
    str_date <- Sys.Date()
    file_list <- list.files(path = glue("data/air_quality/{str_date}/location_data"), pattern = "*.csv", full.names = TRUE)

    # Read and combine all files
    df <- do.call(rbind, lapply(file_list, read.csv))

    particulate <- "pm25 µg/m³"

    df <- filter(df, str_sub(latestDateUTC, 0, 10) == str_date)
    df <- filter(df, name == particulate)

    write.csv(df, glue("data/air_quality/{str_date}/active_pm25_london_sensors_{str_date}.csv"))
}