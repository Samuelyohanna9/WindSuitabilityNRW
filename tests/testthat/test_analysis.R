test_that("parameter validation works correctly", {
  expect_error(
    wind_suitability_analysis(residential_buffer = -100),
    "residential_buffer.*>= 0"
  )

  expect_error(
    wind_suitability_analysis(biotope_min_area = -5),
    "biotope_min_area.*>= 0"
  )
})
