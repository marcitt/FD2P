#loading in the libraries
library(shiny)
library(leaflet)
library(glue)
library(sp)
library(sf)
library(ggplot2)
library(plotly)
library(readr)

#loading in the data
my_sf <- read_sf("https://raw.githubusercontent.com/radoi90/housequest-data/refs/heads/master/london_boroughs.geojson") #london borough geolocational data
borough_data <- read.csv("london-borough-profiles-2016 Data set.csv") #london borough data (population)

#merging the data
colnames(borough_data)[colnames(borough_data) == "Area.name"] <- "name" #rename the columns to be the same 
df <- merge(x = my_sf, y = borough_data, by = "name") #merge into one dataset
df$GLA.Population.Estimate.2016 <- as.numeric(gsub(",", "", df$GLA.Population.Estimate.2016)) #get rid of commas in the data to convert to numeric data
my_sf <- df #update my_sf

#building UI
ui <- fluidPage( #create a responsive webpage
    titlePanel("Urban Wellbeing Dashboard"), #page title
    sidebarLayout(
        sidebarPanel( #panel to add controls and information to the data product
            h2("Location Data"),
            p("Sensor at Westminster Bridge"),
            p("Latitude: 51.492248233302"),
            p("Longitude: -0.147114752900"),
            p(textOutput("pollution")),
            p(textOutput("time")),
            radioButtons( #to prioritize different factors we quantify in our data product
                "test", "Factor Prioritisation:",
                c(
                    "Population" = "pop",
                    "Air Quality" = "air_qual"
                )
            ),
        ),
        mainPanel( #panel where the data visualisation happens
            h4("London"),
            plotlyOutput("plot2"),
            sliderInput(
                inputId = "time_slider",
                label = "Time at sensor:",
                value = 0,
                min = 0,
                max = 24,
                width = "100%"
            )
        )
    ),
)


air_quality_data <- read_csv("open_aq_dataset.csv") #load air quality data

#server logic
server <- function(input, output, session) {
    output$time <- renderText({
        time <- input$time_slider
        glue("Time: {time}:00") #whenever the slider is moved, it will display the new time
    })
    observeEvent(input$time_slider, {
        output$pollution <- renderText({
            value <- air_quality_data$value[input$time_slider] #look up air quality value for that time
            glue("No2 Reading: {value} µg/m³") #display desired air quality data 
        })
    })

#creating the map
    output$plot2 <- renderPlotly({
        p <- ggplot() +
            geom_sf(colour = "#241e55", data = my_sf, size = 1, linewidth = 0.15, aes(fill = GLA.Population.Estimate.2016)) + 
            scale_fill_viridis_c(option = "cyan") +
            coord_sf() #plots the shape of boroughs and colours it based on population density

        ggplotly(p) #making the map interactive
    })
}

shinyApp(ui, server)
