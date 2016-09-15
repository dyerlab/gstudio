context("paternity.R")

test_that("testing",{
  freqs <- c(0.55, 0.30, 0.15, 0.34, 0.34, 0.32)
  loci <- c(rep("TPI",3), rep("PGM",3))
  alleles <- c(LETTERS[1:3],LETTERS[8:10])
  f <- data.frame(Locus=loci, Allele=alleles, Frequency=freqs)
  adults <- make_population(f,N=20)
  offs <- mate( adults[1,], adults[2,], N=10)
  offs$OffID <- offs$ID
  offs$MomID <- 1
  
  
  expect_that( adults, is_a("data.frame"))
  expect_that( offs, is_a("data.frame"))
  
  expect_that( paternity(), throws_error())
  expect_that( paternity( offs), throws_error() )
  expect_that( paternity( offs, adults[1,]), throws_error() )
  
  names(offs)[1] <- "Bob"
  expect_that( paternity( offs, adults[1,], adults), throws_error() )
  names(offs)[1] <- "ID"
  
  p <- paternity( offs, adults[1,], adults )
  expect_that( p, is_a("data.frame"))
  expect_that( dim(p)[1]>0, is_true() )
  expect_that( names(p), is_equivalent_to(c("MomID","OffID","DadID","Fij")))
  expect_that( sum(p$Fij, na.rm=TRUE), equals(10) )
  
  p1 <- paternity( offs, adults[1,], adults, strict=TRUE )
  expect_that( nrow(p)>nrow(p1), is_true())
  
})