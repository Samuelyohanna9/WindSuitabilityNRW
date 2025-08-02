#' Visualize Wind Suitability Results
#'
#' Create an interactive Leaflet map of exclusion zones, suitable areas,
#' and existing turbines from a \code{wind_suitability_results} object.
#'
#' @param result          A \code{wind_suitability_results} object (from \code{wind_suitability_analysis}).
#' @param show_exclusions Logical; include exclusion zones? (default: \code{TRUE})
#' @param show_turbines   Logical; include existing turbines? (default: \code{TRUE})
#' @param output_file     Optional path to save the map as a standalone HTML widget.
#'                        If \code{NULL}, no file is written. (default: \code{NULL})
#' @return A \code{leaflet} map object, invisibly saved to \code{output_file} if provided.
#' @export
#' @import sf
#' @import leaflet
#' @importFrom htmlwidgets saveWidget
visualize_results <- function(
    result,
    show_exclusions = TRUE,
    show_turbines   = TRUE,
    output_file     = NULL
) {
  if (!inherits(result, "wind_suitability_results")) {
    stop("Input must be output from wind_suitability_analysis")
  }

  # Extract layers (may be NULL)
  districts      <- result$districts
  exclusion      <- result$exclusion
  suitable_areas <- result$suitable_areas
  turbines       <- result$turbines

  # Start map
  map <- leaflet::leaflet() %>% leaflet::addTiles()

  # 1) Exclusion zones
  if (show_exclusions && !is.null(exclusion)) {
    excl_ll <- sf::st_transform(exclusion, 4326)
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

  # 2) Suitable areas (prefer suitable_areas, else districts)
  if (!is.null(suitable_areas)) {
    sa_ll <- sf::st_transform(suitable_areas, 4326)
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
  } else if (!is.null(districts)) {
    dist_ll <- sf::st_transform(districts, 4326)
    domain_vals <- dist_ll$suitable_percentage
    pal <- leaflet::colorNumeric("YlGn", domain = domain_vals, na.color = "transparent")
    map <- map %>%
      leaflet::addPolygons(
        data        = dist_ll,
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

  # 3) Turbine points (always use centroids for safety)
  if (show_turbines && !is.null(turbines)) {
    turb_pts <- sf::st_centroid(turbines)
    turb_ll  <- sf::st_transform(turb_pts, 4326)
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

  # Layer control: only for present layers
  groups <- character(0)
  if (show_exclusions && !is.null(exclusion))    groups <- c(groups, "Exclusion Zones")
  if (!is.null(suitable_areas) || !is.null(districts)) groups <- c(groups, "Suitable Areas")
  if (show_turbines && !is.null(turbines))       groups <- c(groups, "Existing Turbines")
  if (length(groups) > 1) {
    map <- map %>%
      leaflet::addLayersControl(
        overlayGroups = groups,
        options       = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }

  # Save to disk if requested
  if (!is.null(output_file)) {
    htmlwidgets::saveWidget(map, file = output_file, selfcontained = TRUE)
  }

  map
}
