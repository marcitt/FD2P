# server.R: Server logic for Shiny app

server <- function(input, output, session) {
    # Reactive calculation for wellbeing raster
    reactive_wellbeing <- reactive({
        air_weight <- input$wellbeing_slider / 100
        green_weight <- 1 - air_weight

        # Recalculate wellbeing metric with updated weights
        wellbeing <- air_weight * air_quality_raster + green_weight * greenspace_raster
        wellbeing <- normalize_raster(1 - wellbeing)

        # Apply threshold filter
        threshold <- input$threshold_slider
        wellbeing[wellbeing < threshold / 10] <- NA

        wellbeing
    })

    # Render Leaflet map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addRasterImage(
                air_quality_raster,
                colors = colorNumeric("YlOrRd", values(air_quality_raster), na.color = NA),
                opacity = 0.7,
                group = "Air Quality"
            ) %>%
            addRasterImage(
                reactive_wellbeing(),
                colors = colorNumeric("RdYlGn", values(air_quality_raster), na.color = "transparent"),
                opacity = 0.7,
                group = "Wellbeing Metric"
            ) %>%
            addPolygons(
                data = greenspace,
                fillColor = "forestgreen",
                color = "darkgreen",
                weight = 1,
                opacity = 0.5,
                fillOpacity = 0.4,
                group = "Public Greenspace"
            ) %>%
            addLegend(
                position = "bottomright",
                pal = colorNumeric("YlOrRd", values(air_quality_raster_unnormalised), na.color = NA),
                values = values(air_quality_raster_unnormalised),
                title = "Air Quality (PM2.5 µg/m3))",
                group = "Air Quality",
                opacity = 1
            ) %>%
            addLegend(
                position = "bottomright",
                pal = colorNumeric("RdYlGn", values(reactive_wellbeing()*10), na.color = "transparent"),
                values = values(reactive_wellbeing()*10),
                title = "Wellbeing Metric",
                group = "Wellbeing Metric",
                opacity = 1
            ) %>%
            addLayersControl(
                overlayGroups = c("Air Quality", "Public Greenspace", "Wellbeing Metric"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            hideGroup("Air Quality") %>%
            hideGroup("Public Greenspace")
    })
}
