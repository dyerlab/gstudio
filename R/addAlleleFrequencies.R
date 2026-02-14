#' Add allele frequency pie charts to a leaflet map
#'
#' This function computes per-stratum allele frequencies for a named locus
#'  and renders them as pie charts on a leaflet map using
#'  \code{leaflet.minicharts::addMinicharts()}.
#' @param data A \code{data.frame} with locus columns, a stratum column,
#'  and coordinate columns.
#' @param locus Character string naming a single locus column (e.g., \code{"TPI"}).
#' @param stratum Column name for population grouping (default \code{"Population"}).
#' @param longitude Column name for longitude coordinates (default \code{"Longitude"}).
#' @param latitude Column name for latitude coordinates (default \code{"Latitude"}).
#' @param map An optional existing leaflet map; if \code{NULL}, creates a new
#'  one with \code{addTiles()}.
#' @param width Pie chart width in pixels (default 40).
#' @param height Pie chart height in pixels (default 40).
#' @param ... Additional arguments passed to \code{leaflet.minicharts::addMinicharts()}
#'  (e.g., \code{opacity}, \code{colorPalette}).
#' @return A leaflet widget with pie charts at each population location.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' \dontrun{
#' data(arapat)
#' addAlleleFrequencies(arapat, locus = "TPI", stratum = "Population")
#' }
addAlleleFrequencies <- function(data, locus, stratum = "Population",
                                 longitude = "Longitude", latitude = "Latitude",
                                 map = NULL,
                                 width = 40, height = 40, ...) {

  # Validate locus exists
  locus_cols <- column_class(data, "locus")
  if (!locus %in% locus_cols)
    stop(paste0("Locus '", locus, "' not found in data. Available loci: ",
                paste(locus_cols, collapse = ", ")))

  if (!requireNamespace("leaflet", quietly = TRUE))
    stop("The 'leaflet' package is required. Install it with install.packages('leaflet').")

  if (!requireNamespace("leaflet.minicharts", quietly = TRUE))
    stop("The 'leaflet.minicharts' package is required. Install it with install.packages('leaflet.minicharts').")

  # Compute per-stratum allele frequencies
  freqs <- frequencies(data, loci = locus, stratum = stratum)

  # Get coordinates per stratum
  coords <- strata_coordinates(data, stratum = stratum,
                               longitude = longitude, latitude = latitude)

  # Reshape to wide matrix: rows = strata, columns = alleles
  wide <- reshape2::dcast(freqs, Stratum ~ Allele, value.var = "Frequency",
                          fill = 0)
  rownames(wide) <- wide$Stratum
  allele_cols <- setdiff(names(wide), "Stratum")

  # Align row order to coords
  wide <- wide[match(coords$Stratum, wide$Stratum), , drop = FALSE]

  # Extract the chart data matrix
  chartdata <- wide[, allele_cols, drop = FALSE]

  # Create map if not provided
  if (is.null(map))
    map <- leaflet::leaflet() |> leaflet::addTiles()

  # Add pie charts

  map <- leaflet.minicharts::addMinicharts(
    map,
    lng = coords$Longitude,
    lat = coords$Latitude,
    chartdata = chartdata,
    type = "pie",
    layerId = coords$Stratum,
    width = width,
    height = height,
    ...
  )

  return(map)
}
