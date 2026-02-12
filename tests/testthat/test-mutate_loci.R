context("mutate_loci.R")

# Helper: build a small test population
.make_test_pop <- function() {
  f1 <- data.frame(
    Population = "A",
    Locus = rep(c("Loc1", "Loc2"), each = 3),
    Allele = rep(c("01", "02", "03"), times = 2),
    Frequency = rep(c(0.5, 0.3, 0.2), times = 2)
  )
  f1$Population <- factor(f1$Population, ordered = TRUE)
  make_populations(f1, N = 30, F = 0)
}


test_that("mutation_model validates rate bounds", {
  expect_error(mutation_model(rate = -0.1, model = "iam"))
  expect_error(mutation_model(rate = 1.5, model = "iam"))
})

test_that("mutation_model requires k for KAM", {
  expect_error(mutation_model(rate = 0.01, model = "kam"))
  expect_error(mutation_model(rate = 0.01, model = "kam", k = 1))
  mm <- mutation_model(rate = 0.01, model = "kam", k = 5)
  expect_s3_class(mm, "mutation_model")
})

test_that("mutate_loci returns unchanged data when rate=0", {
  pop <- .make_test_pop()
  mm <- mutation_model(rate = 0, model = "iam")
  result <- mutate_loci(pop, mm)
  expect_identical(result, pop)
})

test_that("mutate_loci returns unchanged data when mutation=NULL", {
  pop <- .make_test_pop()
  result <- mutate_loci(pop, NULL)
  expect_identical(result, pop)
})

test_that("IAM creates novel alleles", {
  set.seed(42)
  pop <- .make_test_pop()
  original_alleles <- unique(as.character(alleles(pop$Loc1, all = FALSE)))
  mm <- mutation_model(rate = 1.0, model = "iam")
  result <- mutate_loci(pop, mm)
  new_alleles <- unique(as.character(alleles(result$Loc1, all = FALSE)))
  # With rate=1, every allele mutates -> new alleles must exist
  novel <- setdiff(new_alleles, original_alleles)
  expect_true(length(novel) > 0)
})

test_that("SMM shifts numeric alleles by exactly 1", {
  set.seed(123)
  # Create a population with known alleles
  loc_vec <- c(locus(c("05", "05")), locus(c("05", "05")),
               locus(c("05", "05")), locus(c("05", "05")))
  pop <- data.frame(Population = rep("A", 4), Loc1 = loc_vec,
                    stringsAsFactors = FALSE)
  mm <- mutation_model(rate = 1.0, model = "smm")
  result <- mutate_loci(pop, mm)
  # All alleles should be 04 or 06
  new_als <- as.numeric(alleles(result$Loc1, all = FALSE))
  expect_true(all(new_als %in% c(4, 6)))
})

test_that("KAM only produces alleles within 1:k", {
  set.seed(99)
  loc_vec <- c(locus(c("01", "02")), locus(c("02", "03")),
               locus(c("01", "03")), locus(c("03", "03")))
  pop <- data.frame(Population = rep("A", 4), Loc1 = loc_vec,
                    stringsAsFactors = FALSE)
  mm <- mutation_model(rate = 1.0, model = "kam", k = 5)
  result <- mutate_loci(pop, mm)
  new_als <- as.numeric(alleles(result$Loc1, all = FALSE))
  expect_true(all(new_als >= 1 & new_als <= 5))
})

test_that("Missing genotypes are skipped", {
  loc_vec <- c(locus(c("01", "02")), locus(), locus(c("01", "01")))
  pop <- data.frame(Population = rep("A", 3), Loc1 = loc_vec,
                    stringsAsFactors = FALSE)
  mm <- mutation_model(rate = 1.0, model = "iam")
  result <- mutate_loci(pop, mm)
  expect_true(is.na(result$Loc1[2]))
})
