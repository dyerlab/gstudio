#' Create a mutation model specification
#'
#' Creates an object describing a mutation model for use in forward-time
#' simulations. Three models are supported: Infinite Alleles (IAM),
#' K-Allele (KAM), and Stepwise Mutation (SMM).
#'
#' @param rate Per-allele mutation probability on \code{[0,1]}.
#' @param model One of \code{"iam"}, \code{"kam"}, or \code{"smm"}.
#' @param k Number of possible allele states. Required for \code{"kam"} and
#'   must be >= 2.
#' @return A list of class \code{"mutation_model"} with elements
#'   \code{rate}, \code{model}, and \code{k}.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' mm <- mutation_model(rate = 0.001, model = "iam")
#' mm <- mutation_model(rate = 0.001, model = "kam", k = 10)
mutation_model <- function(rate, model = c("iam", "kam", "smm"), k = NULL) {
  model <- match.arg(model)
  if (rate < 0 || rate > 1)
    stop("Mutation rate must be on [0,1].")
  if (model == "kam") {
    if (is.null(k))
      stop("k (number of allele states) is required for the KAM model.")
    if (k < 2)
      stop("k must be >= 2 for the KAM model.")
  }
  ret <- list(rate = rate, model = model, k = k)
  class(ret) <- "mutation_model"
  return(ret)
}


#' Apply mutations to locus columns in a data.frame
#'
#' For each individual and each allele, a Bernoulli draw determines whether
#' a mutation occurs. If so, the allele is replaced according to the specified
#' model (IAM, KAM, or SMM). Missing genotypes are skipped.
#'
#' @param data A \code{data.frame} containing one or more \code{locus} columns.
#' @param mutation A \code{mutation_model} object (or \code{NULL}).
#' @return A \code{data.frame} with mutated locus columns.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
mutate_loci <- function(data, mutation = NULL) {
  if (is.null(mutation) || mutation$rate == 0)
    return(data)
  if (!inherits(mutation, "mutation_model"))
    stop("mutation must be a mutation_model object.")

  locus_cols <- column_class(data, "locus")
  if (any(is.na(locus_cols)))
    return(data)

  for (col in locus_cols) {
    data[[col]] <- .mutate_locus_vector(data[[col]], mutation)
  }
  return(data)
}


# ---------- internal helpers (not exported) ----------

#' Mutate a vector of locus objects
#' @param locus_vec A vector of \code{locus} objects.
#' @param mutation A \code{mutation_model} object.
#' @return A vector of \code{locus} objects.
#' @keywords internal
.mutate_locus_vector <- function(locus_vec, mutation) {
  n <- length(locus_vec)

  # Collect all numeric alleles across the vector for IAM ceiling
  all_alleles <- alleles(locus_vec, all = FALSE)
  all_numeric <- suppressWarnings(as.numeric(all_alleles))
  all_numeric <- all_numeric[!is.na(all_numeric)]

  for (i in seq_len(n)) {
    loc <- locus_vec[i]
    if (is.na(loc))
      next
    als <- alleles(loc)
    if (is.null(als) || length(als) == 0)
      next

    changed <- FALSE
    for (j in seq_along(als)) {
      if (stats::runif(1) < mutation$rate) {
        new_al <- .mutate_allele(als[j], mutation, all_numeric)
        if (!is.null(new_al)) {
          als[j] <- new_al
          changed <- TRUE
          # Update the running max for IAM
          nval <- suppressWarnings(as.numeric(new_al))
          if (!is.na(nval))
            all_numeric <- c(all_numeric, nval)
        }
      }
    }
    if (changed) {
      locus_vec[i] <- locus(als)
    }
  }
  return(locus_vec)
}


#' Mutate a single allele
#' @param allele Character string of the allele.
#' @param mutation A \code{mutation_model} object.
#' @param all_numeric Numeric vector of all alleles seen (for IAM).
#' @return Character string of the new allele, or NULL if skipped.
#' @keywords internal
.mutate_allele <- function(allele, mutation, all_numeric) {
  num_val <- suppressWarnings(as.numeric(allele))
  nchar_allele <- nchar(allele)

  if (mutation$model == "iam") {
    if (is.na(num_val)) {
      # Non-numeric: append a suffix
      return(paste0(allele, "*"))
    }
    new_val <- max(all_numeric, na.rm = TRUE) + 1
    return(.zero_pad(new_val, nchar_allele))
  }

  if (mutation$model == "kam") {
    if (is.na(num_val)) {
      warning("KAM model requires numeric alleles; skipping non-numeric allele.")
      return(NULL)
    }
    possible <- setdiff(seq_len(mutation$k), num_val)
    new_val <- sample(possible, 1)
    return(.zero_pad(new_val, nchar_allele))
  }

  if (mutation$model == "smm") {
    if (is.na(num_val)) {
      warning("SMM model requires numeric alleles; skipping non-numeric allele.")
      return(NULL)
    }
    step <- sample(c(-1, 1), 1)
    new_val <- max(num_val + step, 1)
    return(.zero_pad(new_val, nchar_allele))
  }

  return(NULL)
}


#' Zero-pad a numeric value to match original allele width
#' @param val Numeric value.
#' @param width Target character width.
#' @return Character string, zero-padded.
#' @keywords internal
.zero_pad <- function(val, width) {
  formatted <- formatC(val, width = width, flag = "0", format = "d")
  return(formatted)
}
