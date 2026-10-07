#' Convience function to create population graph object from genotypes.
#' 
#' This function is a convience function that wraps together the subset_loci() (potentially)
#'   function with the popgraph() function (and translations from genotypes to
#'   multivariate data).
#' @param x A \code{data.frame} with \code{locus} objects
#' @param stratum The stratum column to use as the Node designation (default="Population").
#' @param numLoci If not \code{NULL} then how many randomly selected loci (fewer than the total)
#'   to use in the estimation.
#' @param decorate If \code{TRUE}, every numeric column of \code{x} (other
#'   than \code{stratum} and the loci) is averaged within each stratum and added
#'   as a vertex attribute.  With \code{Latitude} and \code{Longitude} columns
#'   this places each node at the barycenter of its sampled individuals, ready
#'   for \code{plot(graph)} and \code{\link{to_sf}}.  Columns named \code{name}
#'   or \code{size} are skipped, as those attributes are set by
#'   \code{\link{popgraph}}.  Default \code{FALSE}; for other summaries or
#'   categorical data use \code{\link{decorate_graph}}.
#' @param ... Other parameters passed to the \code{popgraph()} function.
#' @return A \code{popgraph} object (an \code{igraph} graph).
#' @export
#' @examples
#' data(arapat)
#' g <- population_graph(arapat, stratum = "Population")
#' g
#'
#' # Node coordinates (and any other numeric columns) as vertex attributes
#' g <- population_graph(arapat, decorate = TRUE)
#' igraph::vertex_attr_names(g)

population_graph <- function( x, stratum="Population", numLoci=NULL, decorate=FALSE, ...) {
  if( !is(x,"data.frame")){
    stop("Must pass a data.frame object to this function.")
  }
  stratum <- .detect_stratum(x, stratum, default = "Population", explicit = !missing(stratum))
  x <- .plain_df(x)
  if( !(stratum %in% names(x))) {
    stop("You provided an invalid (or non-existent) column to be used as strata.")
  }
  
  
  if( is.null(numLoci) ) {
    data <- to_mv(x)
  } else {
    df <- subsample_loci(x, numLoci=numLoci)
    data <- to_mv( df )
  }

  groups <- as.factor( x[[stratum]] )
  graph <- popgraph(data,groups,...)

  if( isTRUE(decorate) ) {
    nodes <- igraph::V(graph)$name
    num <- setdiff( names(x)[vapply(x, is.numeric, logical(1))],
                    c(stratum, column_class(x, "locus"), "name", "size") )
    for( col in num ) {
      m <- tapply( x[[col]], groups, mean, na.rm = TRUE )[nodes]
      m[is.nan(m)] <- NA
      graph <- igraph::set_vertex_attr( graph, col, value = as.numeric(m) )
    }
  }

  return( graph )
}