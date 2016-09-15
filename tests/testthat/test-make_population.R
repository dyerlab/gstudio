context("make_population.R")

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
  expect_that( ssfreqs<0.01, is_true())
})