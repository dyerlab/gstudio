context("bad_parents.R")

test_that("testing",{
  freqs <- c(0.55, 0.30, 0.15, 0.34, 0.34, 0.32)
  loci <- c(rep("TPI",3), rep("PGM",3))
  alleles <- c(LETTERS[1:3],LETTERS[8:10])
  f <- data.frame(Locus=loci, Allele=alleles, Frequency=freqs)
  adults <- make_population(f,N=3)
  offs <- mate( adults[1,], adults[2,], N=2)
  offs$ID <- c(1,1)
  offs$OffID <- 1:2
  offs$TPI[2] <- locus(c("D","D"))
  adults$OffID <- 0
  
  data <- rbind( adults[,c(1,4,2,3)], offs[,c(1,4,2,3)])
  data <- data[ order(data$ID, data$OffID),]
  
  p <- bad_parents( data )
  expect_that(p,is_a("data.frame"))
  expect_that(nrow(p),equals(2))
  expect_that(ncol(p),equals(3))
  expect_that(p$PossibleParent,is_equivalent_to(c(TRUE,FALSE)))
})