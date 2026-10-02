#' Internal helper to detect stratum from data or grouping
#'
#' @param x A data.frame or grouped_df.
#' @param stratum The stratum parameter supplied by user (optional).
#' @param default Default stratum value if none detected.
#' @return Character string of stratum column name, or NULL.
#' @noRd
.detect_stratum <- function(x, stratum = NULL, default = NULL) {
  if (inherits(x, "grouped_df")) {
    grps <- dplyr::group_vars(x)
    if (length(grps) > 0) {
      if (is.null(stratum) || (identical(stratum, default) && !(default %in% grps))) {
        return(grps[1])
      }
    }
  }

  if (!is.null(stratum)) {
    return(stratum)
  }

  return(default)
}
