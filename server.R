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
        wellbeing[wellbeing < threshold] <- NA

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
                colors = colorNumeric("RdYlBu", values(air_quality_raster), na.color = "transparent"),
                opacity = 0.7,
                group = "Wellbeing Metric"
            ) %>%
            addLayersControl(
                overlayGroups = c("Air Quality", "Wellbeing Metric"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            hideGroup("Air Quality")
    })
}