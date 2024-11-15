library(shiny)
library(leaflet)
library(glue)

library(sp)

library(sf)
library(ggplot2)

library(plotly)


my_sf <- read_sf("https://raw.githubusercontent.com/radoi90/housequest-data/refs/heads/master/london_boroughs.geojson")
borough_data <- read.csv("london-borough-profiles-2016 Data set.csv")

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
                    "Air Quality" = "air_qual"
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

library(readr)
air_quality_data <- read_csv("openaq_location_225801_measurments.csv")

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
            geom_sf(colour = "#241e55", data = my_sf, size = 1, linewidth = 0.15, aes(fill = GLA.Population.Estimate.2016)) +
            scale_fill_viridis_c(option = "cyan") +
            coord_sf()

        ggplotly(p)
    })
}

shinyApp(ui, server)
