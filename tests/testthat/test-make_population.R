
test_that("testing",{
  freqs <- c(0.55, 0.30, 0.15, 0.34, 0.34, 0.32)
  loci <- c(rep("PGM",3),rep("TPI",3))
  alleles <- c(LETTERS[1:3],LETTERS[8:10])
  f <- data.frame(Locus=loci, Allele=alleles, Frequency=freqs)
  adults <- make_population(f,N=1000)
  
  expect_that( adults, is_a("data.frame"))
  expect_that( column_class(adults,"locus"), is_equivalent_to(c("PGM","TPI")))
  expect_that( dim(adults), is_equivalent_to( c(1000,3)))
  expect_that( names(adults), is_equivalent_to( c("ID","PGM","TPI")))
  
  obs_freqs <- frequencies(adults)
  ssfreqs <- sum( (obs_freqs$Frequency - freqs)^2 )
  expect_true( ssfreqs<0.01)
})


test_that("make_population selects frequency columns by name, not position", {
  f <- data.frame(Allele = c("A", "B", "A", "B"),
                  Locus = rep(c("L1", "L2"), each = 2),
                  Frequency = c(0.5, 0.5, 0.2, 0.8))
  set.seed(1)
  pop <- make_population(f, N = 10)
  expect_equal(names(pop), c("ID", "L1", "L2"))
  expect_true(all(unlist(lapply(c("L1", "L2"), function(l) alleles(pop[[l]]))) %in% c("A", "B")))

  fp <- expand.grid(Allele = c("01", "02"), Locus = c("Loc1", "Loc2"),
                    Population = c("A", "B"), stringsAsFactors = FALSE)
  fp$Frequency <- 0.5
  expect_equal(nrow(make_populations(fp, N = 5)), 10L)
})
