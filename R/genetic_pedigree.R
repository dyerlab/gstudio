#' Inbreeding and kinship from a pedigree
#'
#' This is the front-end function for analyses of a known pedigree.  It
#'  estimates inbreeding coefficients (F), kinship and relatedness from the
#'  pedigree, compares them under an alternative paternity, and draws the
#'  pedigree.
#'
#' The pedigree is a \code{data.frame} with one row per individual and the
#'  columns
#'  \describe{
#'    \item{ID}{A unique key for the individual.}
#'    \item{Sex}{\code{"M"} or \code{"F"}.}
#'    \item{Father, Mother}{The \code{ID} of each parent, \code{NA} (or an
#'      empty string) for founders.  An individual has both parents or
#'      neither.}
#'    \item{Name}{(optional) A display name, used in tables and plots.}
#'    \item{AltFather}{(optional) The \code{ID} of a rumoured or alternative
#'      father; \code{NA} where there is none.}
#'  }
#'  Founders are taken to be unrelated and non-inbred (F = 0), so inbreeding
#'  is underestimated for the descendants of founders who were themselves
#'  related.  The pedigree is checked before any analysis and an error names
#'  the individuals at fault (duplicated IDs, unknown parents, fathers that
#'  are not male or mothers that are not female, a single known parent).
#'
#' The inbreeding coefficient of an individual is the kinship (coancestry)
#'  coefficient of its parents, which is what the chain-counting method sums
#'  over the loops through each common ancestor A,
#'  \eqn{F = \sum (1/2)^i (1 + F_A)}, where \eqn{i} is the number of
#'  individuals in the loop.  Kinship is estimated by \code{kinship2::kinship()}.
#'  Relatedness is twice the kinship, \eqn{r_{XY} = 2\theta_{XY}}; it equals
#'  the textbook values (1/2 for full sibs, 1/4 for half sibs, ...) only when
#'  the individuals' ancestors are not inbred, and an individual's
#'  relatedness to itself is \eqn{1 + F}.
#'
#' @param x A pedigree \code{data.frame} (see Details), e.g.
#'    \code{\link{targaryen}}.
#' @param mode The analysis to perform:
#'    \describe{
#'      \item{inbreeding}{A named numeric vector of F, one per \code{ID}
#'        (default).}
#'      \item{table}{A \code{data.frame} with columns \code{ID}, \code{Name},
#'        \code{F} and, if \code{x} has an \code{AltFather} column,
#'        \code{F_alt} (F under the alternative paternity), sorted from most to
#'        least inbred.}
#'      \item{plot}{Draws the pedigree with \code{kinship2}'s base-graphics
#'        plot method and returns the \code{kinship2} pedigree invisibly.
#'        Squares are males and circles females; double lines join related
#'        mates; individuals with F > 0 are drawn in red and labelled with
#'        their F, and those with F at or above \code{threshold} are filled.
#'        An individual with several mates may be drawn twice, joined by a
#'        dashed line, and individuals with neither parents nor offspring in
#'        the pedigree are not drawn.}
#'      \item{kinship}{The kinship (coancestry) matrix, \eqn{\theta}, with
#'        rows and columns named by \code{ID}.}
#'      \item{relatedness}{The relatedness matrix, \eqn{r = 2\theta}, with
#'        rows and columns named by \code{ID}.}
#'      \item{pedigree}{The pedigree as a \code{kinship2} \code{pedigree}
#'        object, for further analysis with that package.}
#'    }
#' @param father The column holding fathers: \code{"Father"} (default) or
#'    \code{"AltFather"} to analyse the alternative paternity.  Individuals
#'    with no alternative father keep their \code{Father}.  Mode \code{table}
#'    always reports both and ignores this.
#' @param focal (optional) A vector of \code{ID}s.  The pedigree is cut down
#'    to these individuals and all of their ancestors (under both paternities)
#'    before the analysis, e.g. to draw one branch of a large pedigree.  F,
#'    kinship and relatedness depend only on ancestors, so they are unchanged.
#' @param inbred_only For mode \code{table}, keep only the individuals with
#'    F > 0 under either paternity (default \code{TRUE}).
#' @param threshold For mode \code{plot}, the F at or above which a symbol is
#'    filled (default 0.25, the F of an offspring of full siblings with
#'    non-inbred parents).
#' @param labels For mode \code{plot}, the column used to label individuals
#'    (default \code{"Name"} if present, otherwise \code{"ID"}).
#' @param cex For mode \code{plot}, the size of the labels.
#' @param ... Other arguments passed to \code{kinship2}'s plot method in mode
#'    \code{plot}.
#' @return Depends on \code{mode}; see above.
#' @note Requires the \code{kinship2} package.
#' @references Wright, S. (1922). Coefficients of inbreeding and relationship.
#'  The American Naturalist 56: 330-338.
#'
#'  Sinnwell, J. P., Therneau, T. M., & Schaid, D. J. (2014). The kinship2 R
#'  package for pedigree data. Human Heredity 78: 91-93.
#'  \doi{10.1159/000363105}
#' @seealso \code{\link{targaryen}}
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' # Offspring of a full-sibling mating has F = 1/4
#' ped <- data.frame( ID = c("Sire", "Dam", "Son", "Daughter", "Child"),
#'                    Sex = c("M", "F", "M", "F", "M"),
#'                    Father = c(NA, NA, "Sire", "Sire", "Son"),
#'                    Mother = c(NA, NA, "Dam", "Dam", "Daughter") )
#' if( requireNamespace("kinship2", quietly = TRUE) ) {
#'   genetic_pedigree( ped )
#'
#'   data( targaryen )
#'   head( genetic_pedigree( targaryen, mode = "table" ) )
#'   genetic_pedigree( targaryen, mode = "plot", labels = "ID" )
#'
#'   # Relatedness of Rhaenyra's eldest son to his family under each paternity
#'   family <- c( "Jacaerys", "Laenor", "Rhaenyra", "Aegon_III" )
#'   genetic_pedigree( targaryen, mode = "relatedness" )[ family, family ]
#'   genetic_pedigree( targaryen, mode = "relatedness",
#'                     father = "AltFather" )[ family, family ]
#'
#'   # Just the ancestors of Rhaenyra's sons
#'   genetic_pedigree( targaryen, mode = "plot", labels = "ID",
#'                     focal = c( "Jacaerys", "Aegon_III" ) )
#' }
genetic_pedigree <- function( x,
                              mode = c("inbreeding", "table", "plot", "kinship",
                                       "relatedness", "pedigree")[1],
                              father = "Father",
                              focal = NULL,
                              inbred_only = TRUE,
                              threshold = 0.25,
                              labels = NULL,
                              cex = 0.6,
                              ... ) {

  if( !requireNamespace("kinship2", quietly = TRUE) )
    stop("genetic_pedigree() requires the 'kinship2' package: install.packages(\"kinship2\")", call. = FALSE)
  if( length(mode) != 1 )
    stop("genetic_pedigree() takes a single mode.", call. = FALSE)

  x <- .validate_pedigree( x )
  if( !is.null(focal) )
    x <- .pedigree_focal( x, focal )
  mode_clean <- tolower(mode)

  if( mode_clean == "inbreeding" ) {
    return( .pedigree_inbreeding( x, father ) )
  }
  else if( mode_clean == "table" ) {
    ret <- data.frame( ID = x$ID,
                       Name = x$Name,
                       F = unname( .pedigree_inbreeding( x, "Father" ) ),
                       stringsAsFactors = FALSE )
    if( "AltFather" %in% names(x) )
      ret$F_alt <- unname( .pedigree_inbreeding( x, "AltFather" ) )
    if( inbred_only ) {
      keep <- ret$F > 0
      if( !is.null(ret$F_alt) )
        keep <- keep | ret$F_alt > 0
      ret <- ret[ keep, ]
    }
    ret <- ret[ order( -ret$F, ret$ID ), ]
    rownames(ret) <- NULL
    return( ret )
  }
  else if( mode_clean == "plot" ) {
    return( .pedigree_plot( x, father, threshold, labels, cex, ... ) )
  }
  else if( mode_clean == "kinship" ) {
    return( .pedigree_kinship_matrix( x, father ) )
  }
  else if( mode_clean == "relatedness" ) {
    return( 2 * .pedigree_kinship_matrix( x, father ) )
  }
  else if( mode_clean == "pedigree" ) {
    return( .pedigree_kinship2( x, father ) )
  }
  else
    stop("Unrecognized pedigree mode requested: ", mode, call. = FALSE)
}


# Checks a pedigree data.frame and returns it with empty parent fields set
# to NA and a Name column (copied from ID) if it had none.
.validate_pedigree <- function( x ) {
  if( !is( x, "data.frame" ) )
    stop("A pedigree must be a data.frame.", call. = FALSE)
  x <- as.data.frame( x, stringsAsFactors = FALSE )

  missing_cols <- setdiff( c("ID", "Sex", "Father", "Mother"), names(x) )
  if( length(missing_cols) )
    stop("Pedigree is missing column(s): ", paste( missing_cols, collapse = ", " ), call. = FALSE)

  parent_cols <- intersect( c("Father", "Mother", "AltFather"), names(x) )
  for( col in c("ID", parent_cols) ) {
    x[[col]] <- as.character( x[[col]] )
    x[[col]][ !is.na(x[[col]]) & x[[col]] == "" ] <- NA
  }
  x$Sex <- as.character( x$Sex )
  if( !"Name" %in% names(x) )
    x$Name <- x$ID

  if( any( is.na(x$ID) ) )
    stop("Every individual needs an ID.", call. = FALSE)
  dups <- unique( x$ID[ duplicated(x$ID) ] )
  if( length(dups) )
    stop("Duplicated ID(s): ", paste( dups, collapse = ", " ), call. = FALSE)

  bad_sex <- x$ID[ is.na(x$Sex) | !x$Sex %in% c("M", "F") ]
  if( length(bad_sex) )
    stop("Sex must be 'M' or 'F' for: ", paste( bad_sex, collapse = ", " ), call. = FALSE)

  for( col in parent_cols ) {
    unknown <- setdiff( stats::na.omit( x[[col]] ), x$ID )
    if( length(unknown) )
      stop(col, " not found in the ID column: ", paste( unknown, collapse = ", " ), call. = FALSE)
  }

  sex_of <- stats::setNames( x$Sex, x$ID )
  for( col in parent_cols ) {
    expected <- ifelse( col == "Mother", "F", "M" )
    parents <- unique( stats::na.omit( x[[col]] ) )
    wrong <- parents[ sex_of[parents] != expected ]
    if( length(wrong) )
      stop(col, " must be ", ifelse( expected == "M", "male", "female" ), ": ",
           paste( wrong, collapse = ", " ), call. = FALSE)
  }

  one_parent <- x$ID[ is.na(x$Father) != is.na(x$Mother) ]
  if( length(one_parent) )
    stop("Individuals need both parents or neither: ", paste( one_parent, collapse = ", " ), call. = FALSE)

  alt_founders <- character(0)
  if( "AltFather" %in% names(x) )
    alt_founders <- x$ID[ !is.na(x$AltFather) & is.na(x$Mother) ]
  if( length(alt_founders) )
    stop("An AltFather needs a Mother: ", paste( alt_founders, collapse = ", " ), call. = FALSE)

  x
}


# The focal individuals and all of their ancestors (through Father, Mother
# and AltFather), in the original row order.
.pedigree_focal <- function( x, focal ) {
  focal <- as.character( focal )
  unknown <- setdiff( focal, x$ID )
  if( length(unknown) )
    stop("focal not found in the ID column: ", paste( unknown, collapse = ", " ), call. = FALSE)

  parent_cols <- intersect( c("Father", "Mother", "AltFather"), names(x) )
  keep <- focal
  repeat {
    rows <- x$ID %in% keep
    parents <- unique( stats::na.omit( unlist( x[ rows, parent_cols ] ) ) )
    new <- setdiff( parents, keep )
    if( !length(new) )
      break
    keep <- c( keep, new )
  }
  x[ x$ID %in% keep, , drop = FALSE ]
}


# The father of each individual under the chosen paternity.  For
# "AltFather", individuals without an alternative keep their Father.
.pedigree_fathers <- function( x, father = "Father" ) {
  if( !father %in% c("Father", "AltFather") )
    stop("father must be \"Father\" or \"AltFather\".", call. = FALSE)
  if( !father %in% names(x) )
    stop("Column '", father, "' not found in the pedigree.", call. = FALSE)
  ifelse( is.na( x[[father]] ), x$Father, x[[father]] )
}


# The pedigree as a kinship2 pedigree object.
.pedigree_kinship2 <- function( x, father = "Father" ) {
  kinship2::pedigree( id = x$ID,
                      dadid = .pedigree_fathers( x, father ),
                      momid = x$Mother,
                      sex = ifelse( x$Sex == "M", 1, 2 ) )
}


# The kinship matrix, rows and columns in the order of x$ID.
.pedigree_kinship_matrix <- function( x, father = "Father" ) {
  k <- kinship2::kinship( .pedigree_kinship2( x, father ) )
  as.matrix( k[ x$ID, x$ID ] )
}


# F for each individual: the kinship coefficient of its parents.
.pedigree_inbreeding <- function( x, father = "Father" ) {
  dads <- .pedigree_fathers( x, father )
  k <- .pedigree_kinship_matrix( x, father )
  ret <- vapply( seq_len( nrow(x) ), function(i) {
    if( is.na( dads[i] ) ) 0 else k[ dads[i], x$Mother[i] ]
  }, numeric(1) )
  stats::setNames( ret, x$ID )
}


# Draws the pedigree with kinship2 and returns the pedigree invisibly.
.pedigree_plot <- function( x, father = "Father", threshold = 0.25,
                            labels = NULL, cex = 0.6, ... ) {
  if( is.null(labels) )
    labels <- "Name"
  if( !labels %in% names(x) )
    stop("Column '", labels, "' not found in the pedigree.", call. = FALSE)

  ped <- .pedigree_kinship2( x, father )
  F <- .pedigree_inbreeding( x, father )
  ids <- as.character( x[[labels]] )
  ids <- ifelse( F > 0, sprintf( "%s\nF=%.3f", ids, F ), ids )

  old_par <- graphics::par( xpd = NA )
  on.exit( graphics::par( old_par ) )

  # kinship2 notes, and skips, individuals it cannot place (no parents and
  # no offspring, e.g. an AltFather when plotting the official paternity).
  suppressMessages( suppressWarnings(
    plot( ped,
          id = ids,
          affected = as.numeric( F >= threshold ),
          col = ifelse( F > 0, "firebrick", "black" ),
          cex = cex,
          ... )
  ) )

  invisible( ped )
}
