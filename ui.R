ui <- fluidPage(
    titlePanel("Urban Wellbeing Dashboard"),
    sidebarLayout(
        sidebarPanel(
            h4("test"),
            sliderInput(
                "wellbeing_slider",
                "Wellbeing Metric Prioritization:",
                min = 0, max = 100, value = 50,
                post = "% Air Quality vs Greenspace"
            ),
        ),
        mainPanel(
            h4("London"),
            leafletOutput("map"),
        )
    )
)