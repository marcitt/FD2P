library(shiny)
library(leaflet)
library(glue)

library(sp)

library(sf)
library(ggplot2)

library(plotly)

library(readr)

library(data.table)
library(httr)
library(jsonlite)

library("stringr")

library(gstat)  # For interpolation
library(terra)  # For raster creation and manipulation

api_key <- Sys.getenv("OPEN_AQ_API_KEY")


my_sf <- read_sf("https://raw.githubusercontent.com/radoi90/housequest-data/refs/heads/master/london_boroughs.geojson")
borough_data <- read.csv("london-borough-profiles-2016 Data set.csv")
greenspace <- st_read("GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp")  # Load greenspace shapefile
greenspace <- st_transform(greenspace, crs = 4326)  # Reproject to WGS84 (same CRS as Leaflet)


colnames(borough_data)[colnames(borough_data) == "Area.name"] <- "name"
df <- merge(x = my_sf, y = borough_data, by = "name")
df$GLA.Population.Estimate.2016 <- as.numeric(gsub(",", "", df$GLA.Population.Estimate.2016))
my_sf <- df

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
                "test", "Factor Prioritisation:",
                c(
                    "Population" = "pop",
                    "Air Quality" = "air_qual",
                    "Greenspace" = "greenspace"
                )
            ),
        ),
        mainPanel(
            h4("London"),
            plotlyOutput("plot2"),
            sliderInput(
                inputId = "time_slider",
                label = "Time at sensor:",
                value = 0,
                min = 0,
                max = 22,
                width = "100%"
            )
        )
    ),
)

new <- read_csv("current_sensor_data_with_values.csv")
new <- filter(new, name != "Twickenham")

library(readr)
air_quality_data <- read_csv("open_aq_dataset.csv")

# Ensure air_quality_data is an sf object
air_quality_sf <- st_as_sf(
    air_quality_data, 
    coords = c("longitude", "latitude"), 
    crs = 4326
)

# Reproject to a planar CRS
air_quality_sf <- st_transform(air_quality_sf, crs = "EPSG:27700")  # Modern CRS format

# Convert to SpatialPointsDataFrame
air_quality_sp <- sp::SpatialPointsDataFrame(
    coords = st_coordinates(air_quality_sf),  # Extract coordinates
    data = air_quality_sf %>% st_drop_geometry(),  # Drop geometry to get non-spatial data
    proj4string = sp::CRS("EPSG:27700")  # Updated CRS specification
)

# Define the grid for interpolation
london_bbox <- st_bbox(my_sf)
grid <- terra::rast(
    ext(london_bbox[c("xmin", "ymin", "xmax", "ymax")]),
    resolution = 500,
    crs = "+init=epsg:27700"
)

# Perform IDW interpolation
idw_model <- gstat::gstat(
    formula = value ~ 1,
    locations = air_quality_sp,
    set = list(idp = 2)
)

# Create raster from the IDW model
air_quality_raster <- terra::interpolate(grid, idw_model)

# Normalize raster values
air_quality_raster <- (air_quality_raster - min(air_quality_raster[])) / 
                       (max(air_quality_raster[]) - min(air_quality_raster[])
)


server <- function(input, output, session) {
    output$time <- renderText({
        time <- input$time_slider
        glue("Time: {time}:00")
    })
    observeEvent(input$time_slider, {
        output$pollution <- renderText({
            value <- air_quality_data$value[input$time_slider]
            glue("No2 Reading: {value} µg/m³")
        })
    })

    output$plot2 <- renderPlotly({
        p <- ggplot() +
            geom_sf(data = greenspace, fill = "green", alpha = 0.4, colour = "darkgreen", linetype = "dotted") +
            geom_sf(data = my_sf, aes(color = "red"), size = 1, linewidth = 0.15) +
            geom_spatraster(data = air_quality_raster) +  # Add interpolated raster
            coord_sf()

        ggplotly(p)
    })
}

shinyApp(ui, server)
