#' Add allele frequency pie charts to a leaflet map
#'
#' Adds pie charts showing allele frequencies at each sampling location to an
#' existing leaflet map. Designed for use in a pipe after building the map from
#' \code{strata_coordinates()} output.
#'
#' @param map A leaflet map created with \code{leaflet::leaflet(data = coords)},
#'   where \code{coords} is the output of \code{strata_coordinates()}. The
#'   coordinate data is read from \code{map$x$data}.
#' @param freqs A tidy data frame of allele frequencies as returned by
#'   \code{frequencies(data, stratum = "...")}.  Must have columns
#'   \code{Stratum}, \code{Locus}, \code{Allele}, and \code{Frequency}.
#' @param locus Character string naming a single locus to display. Defaults to
#'   \code{NULL}: if \code{freqs} contains exactly one locus it is used
#'   silently; if it contains several, the first is used with a warning.
#' @param width Pie chart width in pixels (default 40).
#' @param height Pie chart height in pixels (default 40).
#' @param ... Additional arguments passed to
#'   \code{leaflet.minicharts::addMinicharts()} (e.g., \code{opacity},
#'   \code{colorPalette}).
#' @return The leaflet map with pie charts added.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' \dontrun{
#' library(leaflet)
#' library(leaflet.minicharts)
#' data(arapat)
#' freqs <- frequencies(arapat, loci = "LTRS", stratum = "Population")
#' strata_coordinates(arapat) |>
#'   leaflet() |>
#'   addTiles() |>
#'   addAlleleFrequencies(freqs)
#' }
addAlleleFrequencies <- function(map, freqs, locus = NULL, width = 40, height = 40, ...) {

  if (!requireNamespace("leaflet", quietly = TRUE))
    stop("The 'leaflet' package is required. Install it with install.packages('leaflet').")
  if (!requireNamespace("leaflet.minicharts", quietly = TRUE))
    stop("The 'leaflet.minicharts' package is required. Install it with install.packages('leaflet.minicharts').")

  # Coordinates must be embedded in the map via leaflet(data = coords)
  coords <- attr(map$x, "leafletData")
  if (is.null(coords))
    stop(
      "No coordinate data found in the map.\n",
      "Build your map as:\n",
      "  strata_coordinates(data) |> leaflet::leaflet() |> leaflet::addTiles() |> addAlleleFrequencies(freqs)"
    )

  # Check that coord strata and frequency strata share at least one level
  coord_strata <- unique(coords$Stratum)
  freq_strata  <- unique(freqs$Stratum)
  overlap      <- intersect(coord_strata, freq_strata)
  if (length(overlap) == 0L)
    stop(
      "No shared strata between coordinates and freqs.\n",
      "  Coord strata: ", paste(coord_strata, collapse = ", "), "\n",
      "  Freq strata:  ", paste(freq_strata,  collapse = ", ")
    )
  if (length(overlap) < length(coord_strata))
    warning(
      length(coord_strata) - length(overlap),
      " coord strata have no matching frequencies and will be skipped: ",
      paste(setdiff(coord_strata, freq_strata), collapse = ", ")
    )

  # Resolve which locus to display
  available_loci <- unique(freqs$Locus)
  if (is.null(locus)) {
    if (length(available_loci) == 1L) {
      locus <- available_loci
    } else {
      warning("Multiple loci in freqs; using '", available_loci[1],
              "'. Specify locus= to suppress this warning.")
      locus <- available_loci[1]
    }
  } else {
    if (!locus %in% available_loci)
      stop("Locus '", locus, "' not found in freqs. Available: ",
           paste(available_loci, collapse = ", "))
  }

  # Subset to the chosen locus and pivot to wide (Stratum × Allele)
  sub <- freqs[freqs$Locus == locus, ]
  wide <- reshape2::dcast(sub, Stratum ~ Allele, value.var = "Frequency", fill = 0)

  # Join coordinates with the wide frequency matrix on Stratum
  joined <- merge(coords, wide, by = "Stratum")
  allele_cols <- setdiff(names(wide), "Stratum")
  chartdata <- joined[, allele_cols, drop = FALSE]

  leaflet.minicharts::addMinicharts(
    map,
    lng       = joined$Longitude,
    lat       = joined$Latitude,
    chartdata = chartdata,
    type      = "pie",
    layerId   = joined$Stratum,
    width     = width,
    height    = height,
    ...
  )
}
