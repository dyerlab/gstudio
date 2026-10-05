#' Internal helper to detect stratum from data or grouping
#'
#' @param x A data.frame or grouped_df.
#' @param stratum The stratum parameter supplied by user (optional).
#' @param default Default stratum value if none detected.
#' @param explicit \code{TRUE} when the caller knows \code{stratum} was supplied
#'   (\code{!missing(stratum)}); \code{NULL} falls back to comparing with \code{default}.
#' @return Character string of stratum column name, or NULL.
#' @noRd
.detect_stratum <- function(x, stratum = NULL, default = NULL, explicit = NULL) {
  # Callers that can tell (via missing()) pass 'explicit'; a stratum the user
  # actually typed always wins over the grouping, even when it equals 'default'.
  if (isTRUE(explicit))
    return(stratum)
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

#' Internal helper: plain data.frame from a tibble or grouped_df
#'
#' Tibbles do not drop to vectors when subset with \code{[}, which breaks code
#' written for data.frames; call this after \code{.detect_stratum()} has read
#' any grouping.
#' @param x A data.frame, tibble, or grouped_df.
#' @return A plain \code{data.frame} (other inputs are returned unchanged).
#' @noRd
.plain_df <- function(x) {
  if (inherits(x, "tbl_df"))
    x <- as.data.frame(dplyr::ungroup(x))
  x
}
