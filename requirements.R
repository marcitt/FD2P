# Requirements.R
# List of required packages
required_packages <- c(
    "shiny",
    "leaflet",
    "glue",
    "sp",
    "gstat",
    "sf",
    "ggplot2",
    "plotly",
    "readr",
    "data.table",
    "httr",
    "jsonlite",
    "raster",
    "dplyr",
    "stringr"
)

# Check and install missing packages
missing_packages <- setdiff(required_packages, installed.packages()[, "Package"])
if (length(missing_packages) > 0) {
    install.packages(missing_packages)
}

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Print confirmation message
cat("All required packages are installed and loaded.\n")
