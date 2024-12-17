ui <- fluidPage(
    # import google font and custom CSS
    tags$head(
        tags$link(
            rel = "stylesheet", 
            href = "https://fonts.googleapis.com/css2?family=Libre+Franklin:wght@300;400;700&display=swap"
        ),
        includeCSS("www/styles.css") 
    ),
    
    # Custom Title
    tags$div("London Outdoor Wellbeing Map", class = "title-panel"),
    
    sidebarLayout(
        sidebarPanel(
            class = "sidebar",  #custom sidebar class
            h2("Personalised Wellbeing Metric"),
            h3("Your personalised wellbeing metric includes a prioritization slider to help you choose how much the metric is affected by Greenspace proximity and PM2.5 density. The treshold slider helps you navigate the best places for your ideal Wellbeing weighting!"),
            
            # Prioritisation Slider with icons and text underneath
            tags$div(class = "slider-wrapper",
                tags$img(src = "tree_icon.svg", class = "icon-left"),  # Left icon
                sliderInput("wellbeing_slider", "Wellbeing Metric Prioritisation" , min = 0, max = 100, value = 50, post = "% Air Quality"),  # Slider
                tags$img(src = "wind_icon.svg", class = "icon-right"),  # Right icon
                tags$div(class = "slider-text",  #text
                    tags$span("Proximity to Greenspace", class = "left-text"),
                    tags$span("Absence of PM2.5", class = "right-text")
                )
            ),

            # Threshold Slider with icons and text underneath
            tags$div(class = "slider-wrapper",
                tags$img(src = "brain_fog_icon.svg", class = "icon-left"),  # Left icon
                sliderInput("threshold_slider", "Wellbeing Metric Threshold" , min = 0, max = 10, value = 0, step = 0.1),  # Slider
                tags$img(src = "wellbeing_icon.svg", class = "icon-right"),  # Right icon
                tags$div(class = "slider-text",  # text
                    tags$span("Anywhere in London", class = "left-text"),
                    tags$span("Best places for outdoor wellbeing", class = "right-text")
                )
            )
        ),
        
        mainPanel(
            leafletOutput("map", height = "600px")
        )
    ),
    
    #decorative leaves
    tags$div(class = "bottom-leaves")
)