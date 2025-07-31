#' Create an interactive Leaflet map of exclusion, suitability, and turbines
#'
#' @param result            A wind_suitability_results object
#' @param show_exclusions   Logical; include exclusion‐zone polygons (default: TRUE)
#' @param show_turbines     Logical; include existing turbine markers (default: TRUE)
#' @param output_file       Optional character; if provided, path to save the HTML widget
#' @return A \pkg{leaflet} map object
#' @export
visualize_results <- function(result,
                              show_exclusions = TRUE,
                              show_turbines   = TRUE,
                              output_file     = NULL) {
  if (!inherits(result, "wind_suitability_results")) {
    stop("Input must be output from wind_suitability_analysis")
  }

  # start map
  map <- leaflet::leaflet() %>% leaflet::addTiles()

  # 1) Exclusion zones
  if (show_exclusions && !is.null(result$exclusion)) {
    excl_sf <- result$exclusion
    excl_ll <- sf::st_transform(excl_sf, 4326)
    map <- map %>%
      leaflet::addPolygons(
        data        = excl_ll,
        fillColor   = "red",
        fillOpacity = 0.4,
        color       = "red",
        weight      = 1,
        label       = ~"Exclusion Zone",
        group       = "Exclusion Zones"
      )
  }

  # 2) Suitable areas
  if (!is.null(result$suitable_areas)) {
    sa <- result$suitable_areas
    sa_ll <- sf::st_transform(sa, 4326)
    # only include numeric domain if present
    domain_vals <- sa_ll$suitable_percentage
    if (is.numeric(domain_vals) && length(domain_vals) > 0) {
      pal <- leaflet::colorNumeric("YlGn", domain = domain_vals, na.color = "transparent")
      map <- map %>%
        leaflet::addPolygons(
          data        = sa_ll,
          fillColor   = ~pal(suitable_percentage),
          fillOpacity = 0.7,
          color       = "#555555",
          weight      = 1,
          label       = ~paste0(NAME, ": ", round(suitable_percentage, 1), "%"),
          group       = "Suitable Areas"
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal      = pal,
          values   = domain_vals,
          title    = "% Suitable",
          group    = "Suitable Areas"
        )
    }
  }

  # 3) Existing turbines
  if (show_turbines && !is.null(result$turbines)) {
    turb_sf <- sf::st_centroid(result$turbines)
    turb_ll <- sf::st_transform(turb_sf, 4326)
    map <- map %>%
      leaflet::addCircleMarkers(
        data        = turb_ll,
        radius      = 4,
        color       = "black",
        fillColor   = "yellow",
        fillOpacity = 0.8,
        label       = ~"Turbine",
        group       = "Existing Turbines"
      )
  }

  # layer control if more than one group
  groups <- c()
  if (show_exclusions && !is.null(result$exclusion)) groups <- c(groups, "Exclusion Zones")
  if (!is.null(result$suitable_areas))                     groups <- c(groups, "Suitable Areas")
  if (show_turbines && !is.null(result$turbines))          groups <- c(groups, "Existing Turbines")
  if (length(groups) > 1) {
    map <- map %>%
      leaflet::addLayersControl(
        overlayGroups = groups,
        options       = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }

  # save to disk if requested
  if (!is.null(output_file)) {
    htmlwidgets::saveWidget(map, file = output_file, selfcontained = TRUE)
  }

  map
}
