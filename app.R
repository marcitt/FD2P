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

api_key <- Sys.getenv("OPEN_AQ_API_KEY")

# GET ALL SENSORS IN LONDON
# res <- GET(
#     "https://api.openaq.org/v3/locations?coordinates=51.508045,-0.128217&radius=25000&limit=1000",
#     add_headers(`X-API-Key` = api_key)
# )

# FORMAT DATA
# openaq_data <- fromJSON(rawToChar(res$content))
# results_data <- as.data.frame(openaq_data$results)
# output_dataframe <- as.data.frame(results_data)

# SAVE FULL DATAFRAME
# write.csv(output_dataframe, "full_sensor_data.csv")

# EXTRACT LONGITUDE, LATITUDE & RECORD OF LATEST READING
# cords <- output_dataframe$coordinates
# latest <- output_dataframe$datetimeLast
# coordinates <- data.frame(output_dataframe$id, output_dataframe$name, cords$latitude, cords$longitude, latest$local)
# colnames(coordinates) <- c("id", "name", "latitude", "longitude", "latest")

# SAVE DATAFRAME
# write.csv(coordinates, "sensor_data.csv")

# GET CURRENT DATE
# date <- Sys.Date()
# str_date <- toString(date)

# FILTER SENSORS RECENTLY TAKING RECORDINGS
# sensor_data <- read.csv("sensor_data.csv")
# current_sensor_data <- filter(sensor_data, str_sub(latest, 0, 10) == str_date)
# write.csv(current_sensor_data, "current_sensor_data.csv")

# # GET ALL SENSOR IDS
# sensors <- read.csv("current_sensor_data.csv")
# ids <- sensors$id

# li <- list()

# # ITERATE THROUGH IDS & GET FIRST SENSOR READING
# for (element in ids) {
#     res <- GET(
#         glue("https://api.openaq.org/v3/locations/{element}/latest"),
#         add_headers(`X-API-Key` = api_key)
#     )
#     openaq_data <- fromJSON(rawToChar(res$content))
#     results_data <- as.data.frame(openaq_data$results)
#     li <- append(li, results_data$value[1])
# }

# # ADD NEW COLUMN FOR SENSOR VALUES
# sensors$value <- c(li)
# sensors %>% filter(name == "Twickenham") #MANUALLY REMOVE OUTLIER

# # https://stackoverflow.com/questions/24829027/unimplemented-type-list-when-trying-to-write-table
# sensors <- apply(sensors, 2, as.character)
# write.csv(sensors, "current_sensor_data_with_values.csv")


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
            geom_sf(colour = "#241e55", data = my_sf, size = 1, linewidth = 0.15, aes(color="red")) + 
            geom_sf(data = greenspace, fill = "green", alpha = 0.4, colour = "darkgreen", linetype = "dotted") +
            # geom_sf(colour = "#241e55", data = my_sf, size = 1, linewidth = 0.15, aes(fill = GLA.Population.Estimate.2016)) +
            scale_fill_viridis_c(option = "cyan") +
            # geom_point(data = coordinates, mapping=aes(x=longitude, y=latitude, color="all sensors")) +
            geom_point(data = new, mapping=aes(x=longitude,y=latitude, color=value)) +
            # geom_point(data = current_sensor_data, mapping=aes(x=longitude,y=latitude, color="active sensors")) +
            coord_sf()

        ggplotly(p)
    })
}

shinyApp(ui, server)
