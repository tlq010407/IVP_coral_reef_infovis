# Coral Reef Data Visualization

## Overview
This project is an Information Visualization initiative focused on empowering coral reef conservation through data visualization. The study provides an extensive dataset of Exclusive Economic Zones (EEZs) for 108 coral reef nations and territories, categorized by reef reliance, adaptive capacity, and societal vulnerability. By leveraging geospatial analysis and interactive visualization techniques, the project aims to aid conservation efforts and policy-making decisions.

## Features
- **Interactive Data Visualizations**: Bubble charts, choropleth maps, histograms, heatmaps, and pie charts.
- **Geospatial Analysis**: Uses GIS techniques to map vulnerability and dependencies.
- **Shiny Web Application**: Provides interactive exploration of coral reef data.
- **Data Processing & Cleaning**: Implements R-based data transformations and filtering.
- **Climate Impact Analysis**: Evaluates sea surface temperature anomalies and coral bleaching events.

## Implementation Details
### 1. Data Preparation
- **Loading Spatial Data**: `st_read()` from the `sf` package to read shapefiles.
- **CSV Data Handling**: `read_csv()` for coral reef-related datasets.
- **Data Cleaning**: Removing non-numeric characters, handling missing values, adjusting longitude and latitude.

### 2. Geographic Data Transformation
- **Coordinate Reference System (CRS) Handling**: Uses `st_transform()` to standardize geographic data.
- **Antimeridian Correction**: Applies `st_wrap_dateline()` to resolve longitude boundary issues.

### 3. Visualization Components
- **Bubble Chart**: Displays relationships between reef dependence, adaptive capacity, and social vulnerability.
- **Choropleth Map**: Shows spatial distribution of reef reliance, adaptive capacity, and vulnerability.
- **Heatmap**: Visualizes coral bleaching alert zones.
- **Pie Chart**: Represents bleaching alert area severity distribution.
- **Histogram**: Analyzes sea surface temperature variations and anomalies.

### 4. Interactive Shiny Application
- **Leaflet Maps**: Provides geographic visualization of vulnerability data.
- **Plotly Charts**: Enables interactive exploration of reef-related datasets.
- **Dynamic User Input**: Allows filtering and zooming into specific vulnerability ranges.

## Requirements
- **Programming Language**: R
- **Key Libraries**: `sf`, `ggplot2`, `plotly`, `shiny`, `leaflet`, `tidyverse`
- **Data Sources**:
  - Coral reef-related datasets (NOAA, WRI reports)
  - Exclusive Economic Zone (EEZ) shapefiles

## How to Run
1. Clone the repository.
2. Install required R libraries.
3. Run the Shiny app using:
   ```r
   library(shiny)
   runApp("app.R")
   ```
4. Explore interactive maps and visualizations.

## Strengths and Weaknesses
### Strengths:
- Provides an intuitive way to analyze coral reef conservation challenges.
- Uses interactive visualizations to enhance engagement.
- Incorporates geospatial and statistical analysis.

### Weaknesses:
- Requires domain expertise to interpret some metrics.
- Data updates may be needed to reflect the latest environmental conditions.

## Future Improvements
- **Machine Learning Integration**: Predictive modeling for future coral bleaching events.
- **Enhanced User Interface**: More customization options for visualizations.
- **Expanded Data Sources**: Incorporate socioeconomic datasets for deeper analysis.

## References
1. NOAA Coral Reef Watch Reports
2. World Resources Institute’s "Reefs at Risk Revisited"
3. R Documentation for `sf`, `shiny`, and `ggplot2`
