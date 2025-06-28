context("make_populations.R")

test_that("testing", {
  library( tidyverse)
  f1 <- data.frame( Population = "A",
                    Locus = rep( c("Loc1","Loc2","Loc3"), each=2 ),
                    Allele = rep( c("01","02"), times = 3 ), 
                    Frequency = c( 0.2, 0.8, 0.3,0.7,0.4,0.6) )
  f2 <- data.frame( Population = "B",
                    Locus = rep( c("Loc1","Loc2","Loc3"), each=2 ),
                    Allele = rep( c("01","02"), times = 3 ), 
                    Frequency = c( 0.3,0.7, 0.2, 0.8,0.4,0.6 ) ) 
  f3 <- data.frame( Population = "C",
                    Locus = rep( c("Loc1","Loc2","Loc3"), each=2 ),
                    Allele = rep( c("01","02"), times = 3 ), 
                    Frequency = c( 0.4,0.6, 0.3,0.7, 0.2, 0.8 ) )
  rbind(f1,f2,f3) |> 
    mutate( Population = factor( Population, ordered=TRUE ) ) -> freqs 
  N <- 20 
  F = 0 
  data <- make_populations(freqs, N, F)
  
  expect_that( data, is_a("data.frame"))
  expect_that( unique(data$Population), is_equivalent_to(c("A","B","C") ) )
  expect_that( dim( data ), is_equivalent_to(c(60,5)))  
})