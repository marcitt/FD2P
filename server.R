server <- function(input, output, session) {
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addRasterImage(
                air_quality_raster,
                colors = colorNumeric("YlOrRd", values(air_quality_raster), na.color = NA),
                opacity = 0.7,
                group = "Air Quality"
            ) %>%
            addPolygons(
                data = greenspace,
                fillColor = "darkgreen",
                color = "darkgreen",
                weight = 1,
                opacity = 0.8,
                fillOpacity = 0.8,
                popup = ~ paste("Greenspace Area"),
                group = "Public Green Spaces"
            ) %>%
            addLayersControl(overlayGroups = c("Public Green Spaces", "Air Quality")) 
    })
}
