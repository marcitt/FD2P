server <- function(input, output, session) {
    reactive_wellbeing <- reactiveVal(wellbeing_raster)

    observeEvent(input$wellbeing_slider, {
        air_weight <- input$wellbeing_slider / 100
        green_weight <- 1 - air_weight

        # Recompute wellbeing metric
        wellbeing_raster <- air_weight * air_quality_raster + green_weight * greenspace_raster
        wellbeing_raster <- (wellbeing_raster - cellStats(wellbeing_raster, "min")) /
            (cellStats(wellbeing_raster, "max") - cellStats(wellbeing_raster, "min"))
        
        # Invert the wellbeing raster
        wellbeing_raster <- 1 - wellbeing_raster

        reactive_wellbeing(wellbeing_raster)
    
    })
    observeEvent(input$threshold_slider, {
        threshold <- input$threshold_slider
        
        # Apply the threshold to the wellbeing raster
        filtered_wellbeing_raster <- wellbeing_raster
        filtered_wellbeing_raster[filtered_wellbeing_raster < threshold] <- NA  # Mask values below threshold
        
        # Update the reactiveVal
        reactive_wellbeing(filtered_wellbeing_raster)
    })

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
                colors = colorNumeric("RdYlBu", values(air_quality_raster), na.color = NA),
                opacity = 0.7,
                group = "Wellbeing Metric"  
            ) %>%
            addRasterImage(
                greenspace_raster,
                colors = colorNumeric("Greens", values(greenspace_raster), na.color = NA, reverse = TRUE),
                opacity = 0.5,
                group = "Greenspace Raster"
            ) %>%
            addPolygons(
                data = greenspace,
                fillColor = "darkgreen",
                color = "darkgreen",
                weight = 1,
                opacity = 0.8,
                fillOpacity = 0.8,
                group = "Public Green Spaces"
            ) %>%
            addLayersControl(
                overlayGroups = c("Public Green Spaces", "Air Quality", "Wellbeing Metric", "Greenspace Raster"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            hideGroup(c("Public Green Spaces", "Air Quality", "Greenspace Raster"))
    })
}


