# WindSuitabilityNRW

**WindSuitabilityNRW** is an R package for transparent, reproducible wind energy suitability analysis in North Rhine-Westphalia, Germany.  
It combines geospatial datasets and regulatory exclusion rules to quantify and visualize land suitable for wind turbine development at the district level.

---

## Features

- **Reproducible spatial analysis** using built-in geodata and user-defined buffer parameters.
- **Visual summaries:** Bar charts, interactive Leaflet maps, and district-level statistics.
- **Export** results to CSV, GeoPackage, and HTML map for use in GIS or reporting.
- **Custom analysis:** Set your own exclusion buffers (residential, highways, airports, etc.).
- **Object-oriented:** S3 classes for results and district reports, with print, summary, plot, and user-defined methods.

---

## Installation

First, install dependencies (if not already present):
```r
install.packages(c("sf", "ggplot2", "dplyr", "leaflet", "tmap"))
```

# From local .tar.gz file
```r
install.packages("WindSuitabilityNRW_0.1.0.tar.gz", repos = NULL, type = "source")
```
## Quick Start
```r

library(WindSuitabilityNRW)

# Run the analysis with default buffer values
result <- wind_suitability_analysis(
  residential_buffer = 500,
  highway_buffer     = 200,
  airport_buffer     = 1000,
  recreation_buffer  = 300,
  biotope_min_area   = 10
)

# Print main results
print(result)

# Plot bar chart of suitable % by district
create_summary_plot(result, plot_type = "bar")

# View interactive map (optional)
# map <- visualize_results(result)
```
## Example Workflow
```r
# Run Suitability Analysis

result <- wind_suitability_analysis(residential_buffer = 400)
Summary and Top Districts

summary(result)
top_districts(result, n = 5)
District Report

dr <- district_report(result, district_name = "Höxter")
print(dr)
plot(dr)
exclusion_summary(dr)
```
## Export Results
```r
export_results(result, output_dir = "wind_results", formats = c("csv", "gpkg"))
```
## Package Functions
wind_suitability_analysis(): Main function for analysis (user can set exclusion distances).

create_summary_plot(): Bar or map plots of district suitability.

visualize_results(): Interactive Leaflet map.

top_districts(): Get top N districts by suitability.

get_district_info(): Retrieve stats for a named district.

district_report(): S3 class for detailed per-district report (with print, plot, and summary).

export_results(): Export to csv/gpkg/html.

extract_summary_table(): Tabular results for reporting.

validate_spatial_data(): Check data structure/CRS.

## Data
The package ships with geospatial data for all NRW districts, wind turbines, exclusion layers (protected areas, buffers), and infrastructure. Data is stored in:


inst/extdata/nrw_wind_energy_data.gpkg

##Vignette
A detailed walk-through of the package—data, methods, maps, and results—is available as a vignette:

browseVignettes("WindSuitabilityNRW")


##License
MIT License (c) 2025 Samuel Yohanna

## Citation
If you use this package in your work, please cite as:

Samuel Yohanna (2025). WindSuitabilityNRW: Wind energy suitability analysis for North Rhine-Westphalia. R package version 0.1.0.

## Contact
Questions or suggestions?
Open an issue or contact Samuelyohanna9 on GitHub.

---
