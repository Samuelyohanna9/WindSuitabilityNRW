#' Generate a static summary plot of suitability
#'
#' @param analysis_result A wind_suitability_results object (output of \code{wind_suitability_analysis})
#' @param plot_type Character; either 'bar' or 'map'
#' @return A ggplot object
#' @export
#' @import ggplot2
#' @importFrom stats reorder
#' @examples
#' \dontrun{
#' result <- wind_suitability_analysis()
#' create_summary_plot(result, "bar")
#' }
create_summary_plot <- function(analysis_result, plot_type = "bar") {
  # Validate input object
  if (!inherits(result, "wind_suitability_results")) {
    stop("Input must be output from wind_suitability_analysis")
  }
  # Validate plot_type
  if (!is.character(plot_type) || length(plot_type) != 1 ||
      !(plot_type %in% c("bar", "map"))) {
    stop("plot_type must be either bar or map")
  }

  # Bar chart
  if (plot_type == "bar") {
    df <- result$summary_stats$district_stats
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = stats::reorder(NAME, suitable_percentage),
        y = suitable_percentage
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Suitability by District",
        x     = "District",
        y     = "% Suitable"
      ) +
      ggplot2::theme_minimal()
    return(p)
  }

  # Map
  if (plot_type == "map") {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package sf required for map plots")
    }
    sf_obj <- result$districts
    if (is.null(sf_obj) || !inherits(sf_obj, "sf")) {
      stop("No district geometry available for map plotting")
    }
    p <- ggplot2::ggplot(sf_obj) +
      ggplot2::geom_sf(ggplot2::aes(fill = suitable_percentage)) +
      ggplot2::scale_fill_viridis_c(
        name   = "% Suitable",
        option = "viridis"
      ) +
      ggplot2::labs(
        title = "Suitability by District"
      ) +
      ggplot2::theme_minimal()
    return(p)
  }
}
