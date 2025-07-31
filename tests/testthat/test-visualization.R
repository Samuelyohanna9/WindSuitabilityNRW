test_that("visualize_results works with mock data", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  # Create mock analysis result
  mock_districts <- data.frame(
    NAME = c("District A", "District B", "District C"),
    total_area_km2 = c(100, 150, 80),
    suitable_area_km2 = c(70, 90, 60),
    excluded_area_km2 = c(30, 60, 20),
    suitable_percentage = c(70, 60, 75),
    excluded_percentage = c(30, 40, 25),
    suitable_pct = c(70, 60, 75),
    gn = c("District A", "District B", "District C")
  )

  # Create simple geometries for testing
  library(sf)
  mock_districts_sf <- st_sf(
    mock_districts,
    geometry = st_sfc(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))),
      st_polygon(list(rbind(c(0,1), c(1,1), c(1,2), c(0,2), c(0,1))))
    ),
    crs = 4326
  )

  mock_exclusion_sf <- st_sf(
    data.frame(id = 1),
    geometry = st_sfc(
      st_polygon(list(rbind(c(0.2,0.2), c(0.8,0.2), c(0.8,0.8), c(0.2,0.8), c(0.2,0.2))))
    ),
    crs = 4326
  )

  mock_turbines_sf <- st_sf(
    data.frame(id = 1:3),
    geometry = st_sfc(
      st_point(c(0.5, 0.5)),
      st_point(c(1.5, 0.5)),
      st_point(c(0.5, 1.5))
    ),
    crs = 4326
  )

  mock_result <- structure(
    list(
      summary_stats = list(
        total_nrw_area = 330,
        total_excluded_area = 110,
        percent_excluded = 33.3,
        district_stats = mock_districts[, c("NAME", "total_area_km2", "suitable_area_km2",
                                            "excluded_area_km2", "suitable_percentage", "excluded_percentage")]
      ),
      districts = mock_districts_sf,
      exclusion = mock_exclusion_sf,
      turbines = mock_turbines_sf,
      parameters = list(
        residential_buffer = 500,
        highway_buffer = 200,
        airport_buffer = 1000,
        recreation_buffer = 300,
        biotope_min_area = 10
      ),
      research_answers = list(
        q1_total_exclusion = "33.3% of NRW land excluded",
        q2_distribution = "Exclusion zones show spatial variation",
        q3_suitable_land = "Average 68.3% land remains suitable"
      )
    ),
    class = "wind_suitability_results"
  )

  # Test that visualization function runs without error
  expect_no_error({
    map <- visualize_results(mock_result)
  })

  # Test that the function returns a leaflet object
  map <- visualize_results(mock_result)
  expect_s3_class(map, "leaflet")

  # Test with different options
  expect_no_error({
    map_no_turbines <- visualize_results(mock_result, show_turbines = FALSE)
    map_no_exclusions <- visualize_results(mock_result, show_exclusions = FALSE)
  })
})

test_that("visualize_results handles missing data gracefully", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  # Create mock result with missing components
  mock_result_minimal <- structure(
    list(
      summary_stats = list(
        total_nrw_area = 100,
        total_excluded_area = 30,
        percent_excluded = 30
      ),
      districts = NULL,
      exclusion = NULL,
      turbines = NULL,
      parameters = list(
        residential_buffer = 500,
        highway_buffer = 200,
        airport_buffer = 1000,
        recreation_buffer = 300,
        biotope_min_area = 10
      )
    ),
    class = "wind_suitability_results"
  )

  # Should still create a map even with missing data
  expect_no_error({
    map <- visualize_results(mock_result_minimal)
  })

  expect_s3_class(visualize_results(mock_result_minimal), "leaflet")
})

test_that("visualize_results validates input correctly", {
  skip_if_not_installed("leaflet")

  # Test with wrong input type
  expect_error(
    visualize_results("not a result object"),
    "Input must be output from wind_suitability_analysis"
  )

  # Test with object missing class
  mock_wrong_class <- list(summary_stats = list())
  expect_error(
    visualize_results(mock_wrong_class),
    "Input must be output from wind_suitability_analysis"
  )
})

test_that("create_summary_plot works with mock data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")

  # Create mock analysis result for plotting
  mock_districts <- data.frame(
    NAME = c("District A", "District B", "District C"),
    total_area_km2 = c(100, 150, 80),
    suitable_area_km2 = c(70, 90, 60),
    excluded_area_km2 = c(30, 60, 20),
    suitable_percentage = c(70, 60, 75),
    excluded_percentage = c(30, 40, 25)
  )

  library(sf)
  mock_districts_sf <- st_sf(
    mock_districts,
    geometry = st_sfc(
      st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
      st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))),
      st_polygon(list(rbind(c(0,1), c(1,1), c(1,2), c(0,2), c(0,1))))
    ),
    crs = 4326
  )

  mock_result <- structure(
    list(
      summary_stats = list(
        district_stats = mock_districts
      ),
      districts = mock_districts_sf
    ),
    class = "wind_suitability_results"
  )

  # Test bar plot
  expect_no_error({
    bar_plot <- create_summary_plot(mock_result, plot_type = "bar")
  })

  bar_plot <- create_summary_plot(mock_result, plot_type = "bar")
  expect_s3_class(bar_plot, "ggplot")

  # Test map plot
  expect_no_error({
    map_plot <- create_summary_plot(mock_result, plot_type = "map")
  })

  map_plot <- create_summary_plot(mock_result, plot_type = "map")
  expect_s3_class(map_plot, "ggplot")
})

test_that("create_summary_plot validates inputs", {
  skip_if_not_installed("ggplot2")

  mock_result <- structure(
    list(summary_stats = list()),
    class = "wind_suitability_results"
  )

  # Test invalid plot type
  expect_error(
    create_summary_plot(mock_result, plot_type = "invalid"),
    "plot_type must be either 'bar' or 'map'"
  )

  # Test wrong input class
  expect_error(
    create_summary_plot("not a result"),
    "Input must be output from wind_suitability_analysis"
  )
})

test_that("file saving works correctly", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")

  # Create minimal mock result
  mock_result <- structure(
    list(
      summary_stats = list(
        total_nrw_area = 100,
        total_excluded_area = 30,
        percent_excluded = 30
      ),
      districts = NULL,
      exclusion = NULL,
      turbines = NULL,
      parameters = list(
        residential_buffer = 500,
        highway_buffer = 200,
        airport_buffer = 1000,
        recreation_buffer = 300,
        biotope_min_area = 10
      )
    ),
    class = "wind_suitability_results"
  )

  # Create temporary file
  temp_file <- tempfile(fileext = ".html")

  expect_no_error({
    map <- visualize_results(mock_result, output_file = temp_file)
  })

  # Check that file was created
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

test_that("visualization functions handle coordinate transformations", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  # Create mock data in a different CRS (UTM Zone 32N - typical for Germany)
  library(sf)
  mock_districts_utm <- st_sf(
    data.frame(
      NAME = "Test District",
      suitable_pct = 65,
      total_area_km2 = 100,
      suitable_area_km2 = 65,
      excluded_percentage = 35
    ),
    geometry = st_sfc(
      st_polygon(list(rbind(c(400000, 5600000), c(401000, 5600000),
                            c(401000, 5601000), c(400000, 5601000), c(400000, 5600000))))
    ),
    crs = 25832  # UTM Zone 32N
  )

  mock_result <- structure(
    list(
      summary_stats = list(
        total_nrw_area = 100,
        total_excluded_area = 35,
        percent_excluded = 35
      ),
      districts = mock_districts_utm,
      exclusion = NULL,
      turbines = NULL,
      parameters = list(
        residential_buffer = 500,
        highway_buffer = 200,
        airport_buffer = 1000,
        recreation_buffer = 300,
        biotope_min_area = 10
      )
    ),
    class = "wind_suitability_results"
  )

  # Should handle CRS transformation automatically
  expect_no_error({
    map <- visualize_results(mock_result)
  })

  expect_s3_class(visualize_results(mock_result), "leaflet")
})


test_that("calculate_buffer_stats returns a buffer_stats object and its methods work", {
  # Create a mock analysis_result with needed parameters
  mock_result <- structure(
    list(parameters = list(
      residential_buffer = 500,
      highway_buffer = 200,
      airport_buffer = 1000,
      recreation_buffer = 300,
      biotope_min_area = 10
    )),
    class = "wind_suitability_results"
  )

  buf <- calculate_buffer_stats(mock_result)
  expect_s3_class(buf, "buffer_stats")
  expect_true(is.data.frame(buf))

  # Test print method
  expect_output(print(buf), "Buffer Constraint Summary")

  # Test summary method
  expect_output(summary(buf), "Summary of buffer constraints")

  # Test plot method (skip if ggplot2 not installed)
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plt <- plot(buf)
    expect_s3_class(plt, "ggplot")
  }
})
