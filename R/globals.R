# For suppressing R CMD check NOTES for non-standard global variables
utils::globalVariables(
  c(
    "NAME", "suitable_percentage", "excluded_percentage", "total_area_km2",
    "suitable_area_km2", "excluded_area_km2", "Constraint", "Buffer.Distance.m",
    "Exclusion.Type", "gn", "."
  )
)
