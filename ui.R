ui <- fluidPage(
    titlePanel("Urban Wellbeing Dashboard"),
    sidebarLayout(
        sidebarPanel(
            h4("Customize Wellbeing Metric"),
            sliderInput(
                "wellbeing_slider",
                "Wellbeing Metric Prioritization:",
                min = 0, max = 100, value = 50,
                post = "% Air Quality vs Greenspace"
            ),
            sliderInput(
                "threshold_slider",
                "Wellbeing Threshold:",
                min = 0, max = 1, value = 0,
                post = "Wellbeing Metric Filteration (0-10)"
            )            
        ),
      
        mainPanel(
            h4("London Wellbeing Map"),
            leafletOutput("map")
        )
    )
)