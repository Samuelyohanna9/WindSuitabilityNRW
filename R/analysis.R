
#' Wind Energy Suitability Analysis for NRW
#'
#' Performs spatial analysis to identify suitable areas for wind energy development
#' in North Rhine-Westphalia using Act on the Determination of Area Requirements for Onshore Wind Energy Systems (Windenergieflächenbedarfsgesetz- WindBG) exclusion criteria.
#'
#' @param residential_buffer Numeric; buffer distance for residential areas in meters (default: 500)
#' @param highway_buffer    Numeric; buffer distance for highways in meters (default: 200)
#' @param airport_buffer    Numeric; buffer distance for airports in meters (default: 1000)
#' @param recreation_buffer Numeric; buffer distance for recreation areas in meters (default: 300)
#' @param biotope_min_area  Numeric; minimum area for biotopes in hectares (default: 10)
#' @param output_map        Logical; if TRUE prints bar chart, static map, and Leaflet map (default: TRUE)
#' @param show_turbines     Logical; if TRUE overlays existing turbines on the suitability maps (default: FALSE)
#' @return List containing analysis results and spatial data
#' @import sf
#' @importFrom dplyr mutate arrange desc select
#' @importFrom purrr map_dbl walk2
#' @importFrom tidyr replace_na
#' @importFrom utils unzip
#' @import ggplot2
#' @import tmap
#' @import leaflet
#' @export
wind_suitability_analysis <- function(
    residential_buffer = 500,
    highway_buffer    = 200,
    airport_buffer    = 1000,
    recreation_buffer = 300,
    biotope_min_area  = 10,
    output_map        = TRUE,
    show_turbines     = FALSE
) {
  ## 1. Check Packages
  message("Checking required packages...")
  pkgs <- c("sf","dplyr","purrr","tidyr","ggplot2","tmap","leaflet")
  purrr::walk(pkgs, ~ if (!requireNamespace(.x, quietly=TRUE))
    stop("Package '", .x, "' required but not installed"))

  ## 2. Unzip Data
  message("Unzipping spatial data...")
  zip_path <- system.file("extdata/nrw_wind_energy_data.zip", package="WindSuitabilityNRW")
  if (!file.exists(zip_path)) stop("Data zip not found: ", zip_path)
  td <- tempfile(); dir.create(td)
  on.exit(unlink(td, recursive=TRUE), add=TRUE)
  utils::unzip(zip_path, exdir=td)

  ## 3. Locate GeoPackage
  message("Locating GeoPackage...")
  gpkg_files <- list.files(td, pattern="\\.gpkg$", full.names=TRUE, recursive=TRUE)
  if (length(gpkg_files)==0) stop("No .gpkg found in ", td)
  gpkg <- gpkg_files[1]
  message("Using GeoPackage: ", gpkg)

  ## 4. Read & Clean Layers
  message("Reading spatial layers...")
  safe_read <- function(path, layer) {
    message("  - Reading layer '", layer, "'")
    x <- sf::st_read(path, layer=layer, quiet=TRUE)
    x <- sf::st_zm(x, drop=TRUE)
    x <- sf::st_make_valid(x)
    gt <- unique(sf::st_geometry_type(x))
    if (all(gt %in% c("POINT","MULTIPOINT","LINESTRING","MULTILINESTRING")))
      x <- sf::st_buffer(x, 0.1)
    if (!all(gt %in% c("POLYGON","MULTIPOLYGON")))
      x <- tryCatch(sf::st_cast(x,"MULTIPOLYGON"), error=function(e)x)
    x
  }
  districts <- safe_read(gpkg, "nrw_districts")
  layers <- list(
    residential = safe_read(gpkg, "nrw_residential_areas_buffer_500m"),
    highways    = safe_read(gpkg, "nrw_federal_highway"),
    airports    = safe_read(gpkg, "nrw_traffic_airports"),
    recreation  = safe_read(gpkg, "nrw_recreation_areas"),
    biotopes    = safe_read(gpkg, "nrw_protected_biotopes"),
    nature      = safe_read(gpkg, "nrw_nature_reserve"),
    holiday     = safe_read(gpkg, "nrw_holiday_areas"),
    settlement  = safe_read(gpkg, "nrw_general_settlement_areas"),
    turbines    = safe_read(gpkg, "nrw_existing_wind_turbines")
  )

  ## 5. CRS Consistency
  message("Ensuring consistent coordinate reference system...")
  crs0 <- sf::st_crs(districts)
  layers <- lapply(layers, function(x) {
    if (sf::st_crs(x) != crs0) x <- sf::st_transform(x, crs0)
    x
  })

  ## 6. Validate Parameters
  message("Validating input parameters...")
  params <- list(
    residential_buffer = residential_buffer,
    highway_buffer     = highway_buffer,
    airport_buffer     = airport_buffer,
    recreation_buffer  = recreation_buffer,
    biotope_min_area   = biotope_min_area
  )
  purrr::walk2(params, names(params), ~{
    if (!is.numeric(.x) || .x < 0) stop(.y, " must be >= 0")
  })

  ## 7. Build & Union Exclusion Zones
  message("Building exclusion buffers...")
  excl_bufs <- list(
    residential = layers$residential,
    highway     = sf::st_buffer(layers$highways,   highway_buffer),
    airport     = sf::st_buffer(layers$airports,   airport_buffer),
    recreation  = sf::st_buffer(layers$recreation, recreation_buffer)
  )
  if (biotope_min_area > 0) {
    layers$biotopes <- layers$biotopes[
      as.numeric(sf::st_area(layers$biotopes)) >= biotope_min_area * 1e4, ]
  }
  excl_list <- c(unname(excl_bufs), layers[c("biotopes","nature","holiday","settlement")])
  message("  - Cleaning geometries...")
  cleaned <- lapply(excl_list, function(x) sf::st_simplify(sf::st_make_valid(x), dTolerance=10))
  message("  - Uniting exclusion zones...")
  union_geom <- tryCatch(
    sf::st_union(do.call(c, lapply(cleaned, sf::st_geometry))),
    error=function(e){
      tmp <- cleaned[[1]]
      for(i in seq_along(cleaned)[-1]) tmp <- sf::st_union(tmp, cleaned[[i]])
      sf::st_geometry(sf::st_sf(geometry=tmp))
    }
  )
  exclusion_union <- sf::st_sf(geometry=union_geom)

  ## 8. Compute District Statistics
  message("Calculating district-level statistics...")
  districts <- suppressWarnings({
    districts %>%
      dplyr::mutate(total_area_km2 = as.numeric(sf::st_area(.)) / 1e6) %>%
      dplyr::mutate(
        excluded_area_km2 = purrr::map_dbl(seq_len(nrow(.)), function(i){
          it <- try(sf::st_intersection(.[i,], exclusion_union), silent=TRUE)
          if (inherits(it, "try-error") || nrow(it)==0) return(0)
          sum(as.numeric(sf::st_area(it))) / 1e6
        }),
        excluded_area_km2   = pmin(total_area_km2, excluded_area_km2),
        excluded_percentage = ifelse(total_area_km2 > 0,
                                     (excluded_area_km2 / total_area_km2) * 100, 0),
        suitable_area_km2   = total_area_km2 - excluded_area_km2,
        suitable_percentage = 100 - excluded_percentage,
        NAME                = gn
      )
  })
  summary_stats <- list(
    total_nrw_area      = sum(districts$total_area_km2, na.rm=TRUE),
    total_excluded_area = sum(districts$excluded_area_km2, na.rm=TRUE),
    percent_excluded    = mean(districts$excluded_percentage, na.rm=TRUE),
    district_stats      = districts %>%
      sf::st_drop_geometry() %>%
      dplyr::select(NAME, total_area_km2, excluded_area_km2,
                    suitable_area_km2, excluded_percentage, suitable_percentage) %>%
      dplyr::arrange(dplyr::desc(suitable_percentage))
  )

  ## 9. Build Suitable-Areas Geometry
  message("Preparing suitable-areas geometry...")
  suitable_areas <- sf::st_difference(districts, exclusion_union)
  turbine_pts <- suppressWarnings(
    sf::st_sf(geometry = sf::st_centroid(sf::st_geometry(layers$turbines)))
  )

  ## 10. Visualization
  if (output_map) {
    message("Generating visualizations...")

    # a) Bar chart
    message("  - Plotting bar chart...")
    df <- summary_stats$district_stats
    bp <- ggplot2::ggplot(df,
                          ggplot2::aes(x = stats::reorder(NAME, suitable_percentage),
                                       y = suitable_percentage)) +
      ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::labs(title="Suitability by District",
                    x="District", y="% Suitable") +
      ggplot2::theme_minimal()
    print(bp)

    # b) Static tmap (suppress migration messages)
    message("  - Plotting static map...")
    suppressMessages({
      tm_suit <- tmap::tm_shape(suitable_areas) +
        tmap::tm_polygons(
          "suitable_percentage",
          palette   = "Greens",
          style     = "cont",
          title     = "% Suitable",
          colorNA   = "transparent"
        ) +
        tmap::tm_layout(
          main.title    = "Potential Suitable Areas for New Turbines",
          legend.outside = TRUE
        )
    })
    print(tm_suit)

    # c) Leaflet
    message("  - Plotting interactive Leaflet map...")
    suit_ll <- sf::st_transform(suitable_areas, 4326)
    turb_ll <- sf::st_transform(turbine_pts,    4326)
    pal     <- leaflet::colorNumeric("Greens", domain=suit_ll$suitable_percentage)
    lf <- leaflet::leaflet(suit_ll) %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(
        fillColor   = ~pal(suitable_percentage),
        fillOpacity = 0.7,
        color       = "#555555",
        weight      = 1,
        label       = ~paste0(NAME, ": ", round(suitable_percentage,1), "%"),
        group       = "Suitable Areas"
      ) %>%
      leaflet::addCircleMarkers(
        data        = turb_ll,
        radius      = 3,
        color       = "black",
        fillColor   = "blue",
        fillOpacity = 0.8,
        label       = ~"Turbine",
        group       = "Existing Turbines"
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        pal      = pal,
        values   = suit_ll$suitable_percentage,
        title    = "% Suitable"
      ) %>%
      leaflet::addLayersControl(
        overlayGroups = c("Suitable Areas","Existing Turbines"),
        options       = leaflet::layersControlOptions(collapsed=FALSE)
      )
    print(lf)
  }

  ## 11. Done
  message("Analysis complete.")
  structure(
    list(
      summary_stats  = summary_stats,
      districts      = districts,
      exclusion      = exclusion_union,
      suitable_areas = suitable_areas,
      turbines       = layers$turbines,
      parameters     = params
    ),
    class="wind_suitability_results"
  )
}
