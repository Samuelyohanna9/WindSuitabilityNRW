
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
  # 1) Validate
  if (!inherits(result, "wind_suitability_results")) {
    stop("Input must be output from wind_suitability_analysis")
  }

  # 2) Extract layers (may be NULL)
  districts <- result$districts
  exclusion <- result$exclusion
  turbines  <- result$turbines

  # 3) Start the map
  m <- leaflet::leaflet() %>% leaflet::addTiles()

  # 4) Add exclusion zones
  if (show_exclusions && inherits(exclusion, "sf")) {
    exc_ll <- sf::st_transform(exclusion, 4326)
    m <- leaflet::addPolygons(
      m, data        = exc_ll,
      color       = "red",
      weight      = 1,
      fillOpacity = 0.2,
      group       = "Exclusion Zones"
    )
  }

  # 5) Add suitable‐area polygons
  if (inherits(districts, "sf")) {
    dist_ll <- sf::st_transform(districts, 4326)
    pal     <- leaflet::colorNumeric("YlGn", domain = dist_ll$suitable_percentage)
    m <- m %>% leaflet::addPolygons(
      data        = dist_ll,
      fillColor   = ~pal(suitable_percentage),
      fillOpacity = 0.7,
      weight      = 1,
      color       = "#444444",
      label       = ~paste0(NAME, ": ", round(suitable_percentage, 1), "%"),
      group       = "Suitable Areas"
    ) %>% leaflet::addLegend(
      position = "bottomright",
      pal      = pal,
      values   = dist_ll$suitable_percentage,
      title    = "% Suitable"
    )
  }

  # 6) Add turbine markers
  if (show_turbines && inherits(turbines, "sf")) {
    turb_ll <- sf::st_transform(turbines, 4326)
    m <- leaflet::addCircleMarkers(
      m, data        = turb_ll,
      radius      = 4,
      color       = "black",
      fillColor   = "yellow",
      fillOpacity = 0.8,
      label       = ~"Turbine",
      group       = "Existing Turbines"
    )
  }

  # 7) Layer control (only layers actually added)
  groups <- c(
    if (show_exclusions  && inherits(exclusion, "sf")) "Exclusion Zones",
    if (inherits(districts, "sf"))                  "Suitable Areas",
    if (show_turbines    && inherits(turbines, "sf")) "Existing Turbines"
  )
  m <- leaflet::addLayersControl(
    m,
    overlayGroups = groups,
    options       = leaflet::layersControlOptions(collapsed = FALSE)
  )

  # 8) Write to file if requested
  if (!is.null(output_file)) {
    htmlwidgets::saveWidget(m, output_file, selfcontained = TRUE)
  }

  # 9) Return the map
  m
}
