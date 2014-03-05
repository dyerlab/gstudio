context("bad_parents.R")

test_that("testing",{
  freqs <- c(0.55, 0.30, 0.15, 0.34, 0.34, 0.32)
  loci <- c(rep("TPI",3), rep("PGM",3))
  alleles <- c(LETTERS[1:3],LETTERS[8:10])
  f <- data.frame(Locus=loci, Allele=alleles, Frequency=freqs)
  adults <- make_population(f,N=20)
  offs <- mate( adults[1,], adults[2,], N=10)
  offs$ID <- 1
  offs$OffID <- 1:10
  adults$OffID <- 0
  
  data <- rbind( adults[,c(1,4,2,3)], offs[,c(1,4,2,3)])
  
  p1 <- bad_parents(data)
  
  expect_that( p1, is_a("data.frame"))
  expect_that( p1$PGM, is_a("locus"))    
  expect_that( all(p1$Is.Parent[ p1$Off!=0]), is_true())

})