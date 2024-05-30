# Load required libraries
library(rgeos)
library(sf)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(shiny)
library(viridis)
library(tidyr)
library(plotly)
# Load the shapefile data
shape_data <- st_read("Social_Vulnerability/Social_Vulnerability.shp")
# Transform to Geographic CRS (WGS 84)
shape_data <- st_transform(shape_data, crs = 4326)
# Apply st_wrap_dateline to handle antimeridian crossing
shape_data <- st_wrap_dateline(shape_data, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
shape_data <- shape_data %>%
  mutate(Adapt_Indx = case_when(
    Adapt_Cat == "High" ~ 4,
    Adapt_Cat == "Medium" ~ 3,
    Adapt_Cat == "Low" ~ 2,
    Adapt_Cat == "Very low" ~ 1,
    Adapt_Cat == "N/A" ~ 0,
    TRUE ~ as.numeric(Adapt_Cat) # Convert the default category to numeric
  ))

# Filter out NA and "N/A" values for 'Vuln_Cat'
bubble_counts <- shape_data %>%
  filter(!is.na(Vuln_Cat) & Vuln_Cat != "N/A") # Filter out NA and "N/A" values for 'Vuln_Cat'
# Preparing data for the interactive bubble chart
data <- bubble_counts %>%
  mutate(
    R_Dep_Indx = round(R_Dep_Indx, 2), 
    Adapt_Indx = round(Adapt_Indx, 2), 
    Vuln_Indx = round(Vuln_Indx, 2),   
    text = paste("Reef Dependence Index: ", R_Dep_Indx, 
                 "<br>Adaptive Capacity Index: ", Adapt_Indx, 
                 "<br>Social Vulnerability Index: ", Vuln_Indx, 
                 "<br>Vulnerability Category: ", Vuln_Cat)
  )
# Define factors for categorical variables
shape_data$R_Dep_Cat <- factor(shape_data$R_Dep_Cat)
shape_data$Adapt_Cat <- factor(shape_data$Adapt_Cat)
shape_data$Vuln_Cat <- factor(shape_data$Vuln_Cat)
# Get levels of categorical variables for dropdown selections
reef_dep_categories <- levels(shape_data$R_Dep_Cat)
adapt_categories <- levels(shape_data$Adapt_Cat)
vuln_categories <- levels(shape_data$Vuln_Cat)
# Identify and fix invalid geometries
invalid <- st_is_valid(shape_data, NA_on_exception = TRUE) == FALSE
cat("Invalid geometries:", sum(invalid), "\n")
valid_shape_data <- shape_data
if (any(invalid)) {
  invalid_indices <- which(invalid)
  for (i in invalid_indices) {
    valid_shape_data[i, ]$geometry <- st_make_valid(valid_shape_data[i, ]$geometry)
  }
}
# Define a color palette for 'Vuln_Cat'
valid_shape_data$Vuln_Cat <- factor(valid_shape_data$Vuln_Cat, levels = c("Very High", "High", "Medium", "Low", "N/A"))
color_palette <- colorFactor(palette = "Set1", domain = valid_shape_data$Vuln_Cat)
# Add a column for marker colors based on 'Vuln_Cat'
valid_shape_data$markerColor <- color_palette(valid_shape_data$Vuln_Cat)
# Define colors and labels for the legend
unique_categories <- sort(unique(valid_shape_data$Vuln_Cat))
legend_colors <- setNames(object = color_palette(unique_categories), nm = unique_categories)
legend_labels <- names(legend_colors)
# Define the UI for the Shiny app
ui <- fluidPage(
  # Application title
  titlePanel("Coral Reef Data Visualization"),
  # Sidebar layout
  selectInput("viewType", "Choose View",
              choices = c("Choropleth Maps" = "choropleth",
                          "Interactive Map" = "interactive",
                          "Bubble Chart of Reef Dependence and Adaptive Capacity" = "interactive_bubble")),
  # Conditional panels based on view type selection
  conditionalPanel(
    condition = "input.viewType == 'choropleth'",
    fluidRow(
      column(6,
             selectInput("mapType", "Select Map Category",
                         c("Reef Dependence" = "R_Dep_Cat",
                           "Adaptive Capacity" = "Adapt_Cat",
                           "Social Vulnerability" = "Vuln_Cat"))
      ),
      column(6,
             uiOutput("categorySelection")
      )
    ),
    leafletOutput("map", height = 600)
  ),
  conditionalPanel(
    condition = "input.viewType == 'interactive'",
    leafletOutput("vulnerabilityMap", height = 600),
    absolutePanel(
      top = 60, right = 50,
      sliderInput("range", "Select Social Vulnerability Index Range",
                  min = min(shape_data$Vuln_Indx, na.rm = TRUE),
                  max = max(shape_data$Vuln_Indx, na.rm = TRUE),
                  step = 1, value = range(shape_data$Vuln_Indx, na.rm = TRUE))
    )
  ),
  conditionalPanel(
    condition = "input.viewType == 'interactive_bubble'",
    plotlyOutput("bubbleChart", height = "600px")
  )
)
# Define the server logic for the Shiny app
server <- function(input, output, session) {
  output$categorySelection <- renderUI({
    if (is.null(input$mapType)) return(NULL)
    choices <- unique(shape_data[[input$mapType]])
    selectInput("category", "Select Category", choices = choices)
  })
  # Filter data based on selected map category and category value
  filteredData <- reactive({
    req(input$mapType, input$category)
    data <- shape_data %>%
      filter(!!sym(input$mapType) == input$category) %>%
      st_transform(crs = 4326)
    return(data)
  })
  # Create a dynamic color palette based on selected map category
  dynamicColorPalette <- reactive({
    req(filteredData())
    data <- filteredData()
    uniqueValues <- unique(data[[input$mapType]])
    colorPalette <- colorFactor(palette = "viridis", domain = uniqueValues)
    return(colorPalette)
  })
  # Render the initial Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })
  # Update the Leaflet map based on selected map category and category value
  observe({
    data <- req(filteredData())
    palette <- req(dynamicColorPalette())
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~palette(data[[input$mapType]]),
        fillOpacity = 0.8,
        weight = 1,
        color = "white",
        popup = ~paste(data[[input$mapType]])
      )
  })
  # Render the interactive vulnerability map
  filtered <- reactive({
    valid_shape_data[valid_shape_data$Vuln_Indx >= input$range[1] & valid_shape_data$Vuln_Indx <= input$range[2], ]
  })
  # Render the initial vulnerability map
  output$vulnerabilityMap <- renderLeaflet({
    createVulnerabilityMap(filtered())
  })
  # Update the vulnerability map based on selected range
  observe({
    data <- filtered()
    if (nrow(data) > 0 && !all(is.na(data$lng)) && !all(is.na(data$lat))) {
      leafletProxy("vulnerabilityMap") %>%
        clearMarkers() %>%
        addMarkers(lng = data$lng, lat = data$lat, popup = paste("Vulnerability Index:", data$Vuln_Indx))
    }
  })
  # Define function to create Leaflet vulnerability map
  createVulnerabilityMap <- function(data, palette = "Set1") {
    # Ensure data is in the expected format
    if (!("geometry" %in% names(data))) {
      stop("Data must contain a 'geometry' column")
    }
    # Convert Vuln_Cat to a factor for color mapping
    data$Vuln_Cat <- factor(data$Vuln_Cat)
    # Define a color palette
    color_palette <- colorFactor(palette = palette, domain = data$Vuln_Cat)
    # Calculate centroids if not already done
    centroids <- suppressWarnings(st_centroid(data))
    # Create and return the Leaflet map
    map <- leaflet(data = data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~ st_coordinates(centroids)[, 1],
        lat = ~ st_coordinates(centroids)[, 2],
        color = ~ color_palette(Vuln_Cat),
        label = ~ as.character(Alt_Name),
        clusterOptions = markerClusterOptions(),
        popup = ~ paste(
          "Country: ", COUNTRY, "<br>",
          "Alternate Name: ", Alt_Name, "<br>",
          "Reef Dependence Index: ", R_Dep_Indx, " (", R_Dep_Cat, ")", "<br>",
          "Adaptive Capacity Index: ", Adapt_Indx, " (", Adapt_Cat, ")", "<br>",
          "Vulnerability Index: ", Vuln_Indx, " (", Vuln_Cat, ")"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = color_palette,
        values = ~ Vuln_Cat,
        title = "Social Vulnerability Category"
      )
    return(map)
  }
  # Render the interactive bubble chart
  output$bubbleChart <- renderPlotly({
    p <- plot_ly(
      data,
      x = ~R_Dep_Indx,
      y = ~Adapt_Indx,
      text = ~text,
      size = ~Vuln_Indx,
      color = ~Vuln_Cat,
      colors = c('Very High' = '#DC143C', 'High' = '#DAA520', 'Medium' = '#00BFFF', 'Low' = '#228B22', 'N/A' ='gray'),
      marker = list(sizemode = 'diameter')
    ) %>%
      add_markers() %>%
      layout(title = 'Interactive Bubble Chart of Reef Dependence and Adaptive Capacity',
             xaxis = list(title = 'Reef Dependence Index'),
             yaxis = list(title = 'Adaptive Capacity Index'),
             showlegend = TRUE)%>%
     layout(legend = list(title = list(text = "Social Vulnerability Category")))
    
    # Return the plotly plot to be rendered
    p
  })
}
# Run the Shiny app
shinyApp(ui = ui, server = server)




# noaa dataset
# Load required libraries
coral_data <- read.csv("noaa.csv")
# Remove non-numeric characters (including commas) from longitude and latitude
coral_data$longitude <- gsub("[^0-9.-]", "", as.character(coral_data$longitude))
coral_data$latitude <- gsub("[^0-9.-]", "", as.character(coral_data$latitude))
# convert to numeric again
coral_data$longitude <- as.numeric(coral_data$longitude)
coral_data$latitude <- as.numeric(coral_data$latitude)
# Remove rows with NA longitude values
coral_data <- coral_data[!is.na(coral_data$longitude), ]
coral_data <- coral_data[!is.na(coral_data$latitude), ]
# Adjust longitudes less than -180 by adding 360
coral_data <- coral_data %>%
  mutate(longitude = if_else(longitude < -180, longitude + 360, longitude))
# Adjust latitude values greater than 90 or less than -90 to NA
coral_data <- coral_data %>%
  mutate(latitude = if_else(latitude > 90 | latitude < -90, NA_real_, latitude))

# 1. Heatmap for Coral Bleaching Alert Areas (CRW_BAA)
# Visualize Bleaching Alert Areas (CRW_BAA) for the single day
ggplot(coral_data, aes(x = longitude, y = latitude, fill = CRW_BAA)) +
  geom_tile() +  
  scale_fill_viridis_c(name = "Bleaching Alert", na.value = "gray", limits = c(0, 4)) +
  labs(title = "Coral Bleaching Alert Areas",
       subtitle = "2024-02-24T12:00:02Z") +
  theme_minimal()

# 2.pie chart CRW_BAA_7D_MAX
# Count the number of occurrences for each 'Vuln_Cat' excluding NA values
max_counts <- coral_data %>%
  filter(!is.na(CRW_BAA_7D_MAX_mask) & CRW_BAA_7D_MAX_mask != "N/A") %>% # Filter out NA values and "NA" strings
  count(CRW_BAA_7D_MAX_mask) %>% 
  mutate(Percentage = n / sum(n) * 100) # Calculate the percentage

# Create the pie chart with percentages
ggplot(max_counts, aes(x="", y= Percentage, fill = CRW_BAA_7D_MAX_mask)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "bleaching heat stress levels",
       title = "Bleaching Alert Area 7-day Maximum Composite") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))  # Add percentage labels


# 3. Distribution of CRW_SST and CRW_SSTANOMALY
# CRW_SST and CRW_SSTANOMALYis numeric
coral_data$CRW_SST <- as.numeric(as.character(coral_data$CRW_SST))
coral_data$CRW_SSTANOMALY <- as.numeric(as.character(coral_data$CRW_SSTANOMALY))
# Convert "NaN" values in CRW_SSTANOMALY to NA, this time ensuring conversion is numeric
coral_data$CRW_SSTANOMALY[is.na(coral_data$CRW_SSTANOMALY)] <- NA
# Remove rows with NAs in both CRW_SST and CRW_SSTANOMALY
coral_data_clean <- na.omit(coral_data[, c("CRW_SST", "CRW_SSTANOMALY")])
# Reshape the data into long format
coral_data_long <- coral_data_clean %>%
  pivot_longer(
    cols = c(CRW_SST, CRW_SSTANOMALY),
    names_to = "Temperature_Type",
    values_to = "Value"
  )

# Reshape the data into long format
coral_data_long <- coral_data_clean %>%
  pivot_longer(
    cols = c(CRW_SST, CRW_SSTANOMALY),
    names_to = "Temperature_Type",
    values_to = "Value"
  )

# Reshape data to long format
coral_data_long <- coral_data_clean %>%
  pivot_longer(cols = c(CRW_SST, CRW_SSTANOMALY), names_to = "Temperature_Type", values_to = "Value")

# Plotting the histograms overlaid on one graph showing percentages
ggplot(coral_data_long, aes(x = Value, fill = Temperature_Type)) +
  geom_histogram(data = coral_data_long %>% filter(Temperature_Type == "CRW_SST"),
                 aes(y = ..count../sum(..count..)), 
                 binwidth = 0.5, alpha = 0.7, position = 'identity') +
  geom_histogram(data = coral_data_long %>% filter(Temperature_Type == "CRW_SSTANOMALY"),
                 aes(y = ..count../sum(..count..)), 
                 binwidth = 0.5, alpha = 0.7, position = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("CRW_SST" = "blue", "CRW_SSTANOMALY" = "red")) +
  labs(title = "Distribution of CRW_SST and CRW_SSTANOMALY",
       x = "Temperature (°C)",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        legend.position = "bottom")  # Move legend to the bottom
