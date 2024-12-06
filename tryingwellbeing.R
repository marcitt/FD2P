# Load required libraries
library(shiny)       # For creating the Shiny web app
library(leaflet)     # For rendering interactive maps
library(glue)        # For string interpolation
library(sp)          # For spatial data handling
library(sf)          # For reading and working with shapefiles
library(ggplot2)     # For creating plots
library(plotly)      # For making ggplot interactive
library(readr)       # For reading CSV files
library(data.table)  # For fast data manipulation
library(httr)        # For HTTP requests (used for API calls)
library(jsonlite)    # For parsing JSON data
library(stringr)     # For string manipulation

# Retrieve the API key from environment variables (OpenAQ API)
api_key <- Sys.getenv("OPEN_AQ_API_KEY")

# Load spatial data for London boroughs (GeoJSON format)
my_sf <- read_sf("https://raw.githubusercontent.com/radoi90/housequest-data/refs/heads/master/london_boroughs.geojson")

# Load borough attribute data (e.g., population)
borough_data <- read.csv("london-borough-profiles-2016 Data set.csv")

# Load greenspace shapefile data
greenspace <- st_read("GiGL_SpacesToVisit_Open_Shp/GiGL_SpacesToVisit_region.shp")  # Greenspace polygons
greenspace <- st_transform(greenspace, crs = 4326)  # Reproject to WGS84 (common CRS for Leaflet maps)

# Load the air quality sensor data
new <- read_csv("current_sensor_data_with_values.csv")

# Filter out irrelevant or noisy data (e.g., "Twickenham" as an outlier)
new <- filter(new, name != "Twickenham")

# Load additional air quality data
air_quality_data <- read_csv("open_aq_dataset.csv")

# Rename columns in borough data for consistency
colnames(borough_data)[colnames(borough_data) == "Area.name"] <- "name"

# Merge spatial and attribute data by borough name
df <- merge(x = my_sf, y = borough_data, by = "name")

# Convert population estimates from strings to numeric
df$GLA.Population.Estimate.2016 <- as.numeric(gsub(",", "", df$GLA.Population.Estimate.2016))

# Update the spatial data with merged data
my_sf <- df

# Normalize Air Quality Data
air_quality_data$air_norm <- air_quality_data$value / max(air_quality_data$value, na.rm = TRUE)

# Normalize Greenspace Data
greenspace$area_norm <- greenspace$AreaHa / max(greenspace$AreaHa, na.rm = TRUE)

# Define the user interface (UI)
ui <- fluidPage(
    titlePanel("Urban Wellbeing Dashboard"),  # Title of the app
    sidebarLayout(
        sidebarPanel(
            h2("Location Data"),  # Sidebar title
            p("Sensor at Westminster Bridge"),  # Static information about a location
            p("Latitude: 51.492248233302"),      # Static latitude
            p("Longitude: -0.147114752900"),     # Static longitude
            p(textOutput("pollution")),          # Display NO2 pollution levels
            p(textOutput("time")),               # Display selected time from slider
            radioButtons(                       # Allow user to choose which dataset to prioritize
                "test", "Factor Prioritisation:",
                c(
                    "Population" = "pop",       # Option to prioritize population data
                    "Air Quality" = "air_qual", # Option to prioritize air quality data
                    "Greenspace" = "greenspace" # Option to prioritize greenspace data
                )
            ),
            sliderInput(
                "wellbeing_slider",
                label = div(style = "text-align: center;",
                            span("air quality", style = "float: left;"),
                            span("green space", style = "float: right;")
                ),
                min = 0,
                max = 100,
                value = 50,  # Default 50% weight for both
                step = 1
            )
        ),
        mainPanel(
            h4("London"),                        # Title of the map
            plotlyOutput("plot2"),               # Interactive map output
            sliderInput(                         # Slider to select the time for pollution data
                inputId = "time_slider",
                label = "Time at sensor:",
                value = 0,                       # Default value
                min = 0,                         # Minimum slider value
                max = 22,                        # Maximum slider value
                width = "100%"                   # Full-width slider
            )
        )
    )
)

# Define the server logic
server <- function(input, output, session) {
    # Render the selected time from the slider
    output$time <- renderText({
        time <- input$time_slider  # Get the slider value
        glue("Time: {time}:00")    # Format the time as a string
    })
    
    # Update the NO2 pollution level dynamically based on slider input
    observeEvent(input$time_slider, {
        output$pollution <- renderText({
            value <- air_quality_data$value[input$time_slider]  # Get pollution value for selected time
            glue("No2 Reading: {value} µg/m³")                  # Format the pollution data as a string
        # Calculate the wellbeing metric based on user preferences
    observeEvent(input$wellbeing_slider, {
        # Calculate weights from the slider
        air_weight <- input$wellbeing_slider / 100
        green_weight <- 1 - air_weight
        
        # Apply weights to the normalized data
        wellbeing_metric <- air_quality_data$air_norm * air_weight + greenspace$area_norm * green_weight
        
        # Add wellbeing_metric as a column to your combined dataset or shapefile
        borough_data$wellbeing_metric <- wellbeing_metric
    })
        })
    })

    # Render the interactive map
    output$plot2 <- renderPlotly({
        p <- ggplot() +
            geom_sf(data = greenspace, aes(fill = area_norm), colour = "green", alpha = 0.5) + # Add greenspace layer
            geom_point(data = air_quality_data, aes(x = longitude, y = latitude, color = air_norm), size = 3) + # Add air quality points
            scale_fill_gradient(low = "blue", high = "green", name = "Greenspace") + # Gradient for greenspace
            scale_color_gradient(low = "yellow", high = "red", name = "Air Quality") + # Gradient for air quality
            geom_sf(data = borough_data, aes(fill = wellbeing_metric), alpha = 0.6) + # Add wellbeing metric layer
            coord_sf() # Coordinate system for map visualization

        ggplotly(p) # Convert ggplot to an interactive Plotly map
    })
}

# Launch the Shiny app
shinyApp(ui, server)