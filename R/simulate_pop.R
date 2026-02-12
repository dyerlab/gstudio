#' Forward-time stochastic population simulation
#'
#' Runs a multi-generation forward-time simulation with optional mutation,
#' selfing, migration (with temporal regime changes), and census snapshots.
#' Each generation cycle: census, migrate, mate, mutate.
#'
#' @param pop A \code{data.frame} with at least one \code{locus} column and
#'   a stratum column (the initial generation).
#' @param ngen Number of generations to simulate.
#' @param mutation A \code{mutation_model} object or \code{NULL} (no mutation).
#' @param selfing_rate Fraction of selfing on \code{[0,1]}, passed to
#'   \code{mixed_mating()}.
#' @param migration A single \code{migration_event}, a list of them, or
#'   \code{NULL} (no migration).
#' @param census_interval Save a snapshot every N generations, or \code{NULL}
#'   for no census.
#' @param census_dir Directory in which to save \code{.rds} census files.
#'   Created if it does not exist.
#' @param stratum Name of the population column (default \code{"Population"}).
#' @param verbose Print progress messages (default \code{TRUE}).
#' @return The final-generation \code{data.frame}.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' \dontrun{
#' f <- data.frame(
#'   Population = rep(c("A","B"), each = 4),
#'   Locus = rep(c("L1","L1"), each = 4),
#'   Allele = rep(c("01","02"), times = 4),
#'   Frequency = rep(c(0.5, 0.5), times = 4)
#' )
#' pop <- make_populations(f, N = 20)
#' result <- simulate_pop(pop, ngen = 10, verbose = FALSE)
#' }
simulate_pop <- function(pop, ngen, mutation = NULL, selfing_rate = 0,
                         migration = NULL, census_interval = NULL,
                         census_dir = NULL, stratum = "Population",
                         verbose = TRUE) {

  # --- input validation ---
  if (!inherits(pop, "data.frame"))
    stop("pop must be a data.frame.")
  if (!(stratum %in% names(pop)))
    stop(paste0("Cannot find stratum column '", stratum, "' in pop."))
  if (ngen < 1)
    stop("ngen must be >= 1.")
  if (selfing_rate < 0 || selfing_rate > 1)
    stop("selfing_rate must be on [0,1].")

  # Wrap a single migration_event in a list
  if (inherits(migration, "migration_event"))
    migration <- list(migration)

  # Record target N per population so we maintain constant size
  pop_sizes <- table(pop[[stratum]])

  # --- main loop ---
  for (gen in seq_len(ngen)) {

    # 1. Census
    if (!is.null(census_interval) && !is.null(census_dir)) {
      if (gen %% census_interval == 0) {
        .save_census(pop, gen, census_dir)
      }
    }

    # 2. Migrate
    mig_mat <- .get_migration_matrix(gen, migration)
    if (!is.null(mig_mat)) {
      pop <- migrate(pop, stratum = stratum, m = mig_mat)
    }

    # 3. Mate — per population, constant N
    pops <- partition(pop, stratum)
    offspring_list <- list()
    for (pname in names(pops)) {
      target_n <- as.integer(pop_sizes[pname])
      pop_i <- pops[[pname]]

      # Generate offspring using mixed_mating
      off <- mixed_mating(pop_i, N = 1, s = selfing_rate)

      # Resample to target N if sizes diverged
      if (nrow(off) != target_n) {
        idx <- sample(seq_len(nrow(off)), size = target_n, replace = TRUE)
        off <- off[idx, , drop = FALSE]
      }
      off[[stratum]] <- pname
      offspring_list[[pname]] <- off
    }
    pop <- do.call(rbind, offspring_list)
    rownames(pop) <- NULL

    # 4. Mutate
    if (!is.null(mutation)) {
      pop <- mutate_loci(pop, mutation)
    }

    if (verbose && gen %% max(1, ngen %/% 10) == 0)
      message(paste0("Generation ", gen, "/", ngen))
  }

  # Save final generation as census
  if (!is.null(census_dir)) {
    .save_census(pop, ngen, census_dir)
  }

  return(pop)
}


# ---------- internal helpers ----------

#' Look up the active migration matrix for a given generation
#' @param gen Current generation number.
#' @param events List of migration_event objects (or NULL).
#' @return A migration matrix or NULL.
#' @keywords internal
.get_migration_matrix <- function(gen, events) {
  if (is.null(events))
    return(NULL)
  for (evt in events) {
    in_range <- gen >= evt$start
    if (!is.null(evt$end))
      in_range <- in_range && gen <= evt$end
    if (in_range)
      return(evt$matrix)
  }
  return(NULL)
}


#' Save a census snapshot as an .rds file
#' @param pop data.frame of the population.
#' @param gen Generation number.
#' @param census_dir Directory to write to.
#' @keywords internal
.save_census <- function(pop, gen, census_dir) {
  if (!dir.exists(census_dir))
    dir.create(census_dir, recursive = TRUE)
  fname <- file.path(census_dir, sprintf("gen_%04d.rds", gen))
  saveRDS(pop, file = fname)
}
