library(shiny)
library(leaflet)
library(sf)
library(readr)
library(gstat)
library(sp)
library(raster)
library(dplyr)

# Load greenspace shapefile
greenspace <- st_read("GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp")  # Load greenspace shapefile
greenspace <- st_transform(greenspace, crs = 4326)  # Reproject to WGS84

# Load borough data
my_sf <- read_sf("https://raw.githubusercontent.com/radoi90/housequest-data/refs/heads/master/london_boroughs.geojson")

# Load the correct air quality dataset
air_quality_data <- read_csv("current_sensor_data_with_values.csv")

# Filter out irrelevant or noisy data (e.g., "Twickenham" as an outlier)
air_quality_data <- filter(air_quality_data, name != "Twickenham")

# Convert air quality data to a spatial object
air_quality_sf <- st_as_sf(
    air_quality_data,
    coords = c("longitude", "latitude"),
    crs = 4326  # WGS84
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
    resolution = 500,  # Set grid cell size
    crs = sp::CRS("+init=epsg:27700")  # British National Grid
)

# Perform IDW interpolation
idw_model <- gstat(
    formula = value ~ 1,
    locations = air_quality_sp,
    set = list(idp = 2)  # Power parameter
)

# Interpolate the grid
air_quality_raster <- interpolate(grid, idw_model)

# Normalize raster values
air_quality_raster <- (air_quality_raster - cellStats(air_quality_raster, "min")) /
                      (cellStats(air_quality_raster, "max") - cellStats(air_quality_raster, "min"))

# Define UI
ui <- fluidPage(
    titlePanel("Urban Wellbeing Dashboard"),
    sidebarLayout(
        sidebarPanel(
            h2("Location Data"),
            p("Sensor at Westminster Bridge"),
            p("Latitude: 51.492248233302"),
            p("Longitude: -0.147114752900"),
            p(textOutput("pollution")),
            p(textOutput("time")),
            radioButtons(
                "test", "Data Layers:",
                c(
                    "Air Quality" = "air_qual",
                    "Greenspace" = "greenspace",
                    "Wellbeing Metric" = "Wellbeing Metric"
                )
            ),
            sliderInput(
                "wellbeing_slider",
                "Wellbeing Metric Prioritization:",
                min = 0, max = 100, value = 50,
                post = "% Air Quality vs Greenspace"
            )
        ),
        mainPanel(
            h4("London"),
            leafletOutput("map"),
            sliderInput(
                inputId = "time_slider",
                label = "Time at sensor:",
                value = 0,
                min = 0,
                max = 24,
                width = "100%"
            ),
            
        )
    )
)

# Define server
server <- function(input, output, session) {
    observeEvent(input$wellbeing_slider, {
        air_weight <- input$wellbeing_slider / 100
        green_weight <- 1 - air_weight
        greenspace$wellbeing_metric <- green_weight  # Add metric for greenspace (dummy example)
    })

    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addRasterImage(
                air_quality_raster,
                colors = colorNumeric("RdYlGn", values(air_quality_raster), na.color = NA),
                opacity = 0.7
            ) %>%
            addPolygons(
                data = greenspace,
                fillColor = "forestgreen",
                color = "darkgreen",
                weight = 1,
                opacity = 0.5,
                fillOpacity = 0.4,
                popup = ~paste("Greenspace Area")
            )
    })
}

# Run the app
shinyApp(ui, server)