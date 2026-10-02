
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
#' @examples
#' data(arapat)
#' mm <- mutation_model(rate = 0.01, model = "iam")
#' arapat_mut <- mutate_loci(arapat[1:10, ], mutation = mm)
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

  # Separate counter for non-numeric IAM novel alleles so each mutation event
  # produces a unique label rather than accumulating "*" suffixes
  novel_count <- 0L

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
        new_al <- .mutate_allele(als[j], mutation, all_numeric, novel_count)
        if (!is.null(new_al)) {
          als[j] <- new_al
          changed <- TRUE
          nval <- suppressWarnings(as.numeric(new_al))
          if (!is.na(nval))
            all_numeric <- c(all_numeric, nval)
          else
            novel_count <- novel_count + 1L
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
#' @param novel_count Integer count of non-numeric IAM novel alleles created so far.
#' @return Character string of the new allele, or NULL if skipped.
#' @keywords internal
.mutate_allele <- function(allele, mutation, all_numeric, novel_count = 0L) {
  num_val <- suppressWarnings(as.numeric(allele))
  nchar_allele <- nchar(allele)

  if (mutation$model == "iam") {
    if (is.na(num_val)) {
      # Non-numeric: generate a unique novel label rather than appending "*"
      # (star-suffix accumulation makes "A" -> "A*" -> "A**" across generations,
      # violating IAM's requirement that every mutation event produces a new allele)
      return(paste0("iam_", novel_count + 1L))
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
