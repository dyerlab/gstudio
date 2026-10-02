#' Information-loss reference for the gravity asymmetry index
#'
#' @description
#' As loci fix, Population Graphs are estimated from fewer informative loci, and
#' noisier edge weights inflate \eqn{|\Delta|} whatever the direction of gene flow.
#' This function measures that inflation for a given data set: it rebuilds the
#' Population Graph from \code{nsub} random subsets of \code{k} polymorphic loci for
#' each \code{k}, and records the graph-mean \eqn{|\Delta|} together with the
#' polymorphism of the loci used.  Comparing a data set's own \eqn{|\Delta|} with
#' this curve at matched within-population polymorphism shows how much of it
#' information loss alone could produce.  Run it on data believed to be free of
#' directional gene flow (or on the data at hand, as a sensitivity check); it is a
#' diagnostic, not a calibrated threshold.
#'
#' @param x A \code{data.frame} with \code{locus} columns and a stratum column.
#' @param stratum Name of the stratum column (default \code{"Population"}).
#' @param k Numbers of loci to sample (values larger than the number of polymorphic
#'   loci are skipped).
#' @param nsub Random subsets per value of \code{k} (default 3).
#' @param gamma Bandwidth multiplier for \eqn{\Delta} (default \code{0.5}).
#' @param ... Passed to \code{\link{popgraph}}.
#' @return A \code{data.frame}, one row per subset: \code{k}, \code{subset},
#'   \code{n_poly_global} (loci polymorphic in the whole sample), \code{n_poly_pop}
#'   (mean number of loci polymorphic within a stratum), \code{He} (mean within-stratum
#'   expected heterozygosity over the loci used), \code{n_edges},
#'   \code{mean_abs_delta}, \code{ok} (graph built).
#' @seealso \code{\link{source_sink_scores}}, \code{\link{popgraph}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' \donttest{
#' data(arapat)
#' set.seed(1)
#' locus_subsample_null(arapat, k = c(3, 5, 8), nsub = 1)
#' }
#' @export
locus_subsample_null <- function(x, stratum = "Population", k = c(2:6, 8, 10, 12), nsub = 3, gamma = 0.5, ...) {
  if (!inherits(x, "data.frame") || !(stratum %in% names(x))) stop("'x' must be a data.frame with the stratum column")
  loci <- names(x)[vapply(x, function(v) inherits(v, "locus"), logical(1))]
  if (!length(loci)) stop("no locus columns found")
  fr <- frequencies(x[, c(stratum, loci)], stratum = stratum)
  pbar <- stats::aggregate(Frequency ~ Locus + Allele, data = fr, FUN = mean)
  poly <- unique(pbar$Locus[pbar$Frequency > 0 & pbar$Frequency < 1])
  poly <- loci[loci %in% poly]
  out <- list()
  for (kk in k[k <= length(poly)]) for (s in seq_len(nsub)) {
    L <- sample(poly, kk)
    f <- fr[fr$Locus %in% L, ]
    pl <- tapply(f$Frequency, list(f$Stratum, f$Locus), function(v) any(v > 0 & v < 1))
    he <- tapply(f$Frequency, list(f$Stratum, f$Locus), function(v) 1 - sum(v^2))
    gb <- stats::aggregate(Frequency ~ Locus + Allele, data = f, FUN = mean)
    res <- tryCatch({
      gph <- popgraph(to_mv(x[, L, drop = FALSE]), groups = as.factor(x[[stratum]]), ...)
      ed <- gravity_edges(gph, gamma)
      c(n_edges = nrow(ed), mean_abs_delta = mean(abs(ed$Delta)), ok = 1)
    }, error = function(e) c(n_edges = NA, mean_abs_delta = NA, ok = 0))
    out[[length(out) + 1]] <- data.frame(k = kk, subset = s,
      n_poly_global = length(unique(gb$Locus[gb$Frequency > 0 & gb$Frequency < 1])),
      n_poly_pop = mean(rowSums(pl, na.rm = TRUE)), He = mean(he, na.rm = TRUE), t(res))
  }
  do.call(rbind, out)
}
