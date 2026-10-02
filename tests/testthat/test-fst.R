
test_that("checking",{
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ) )
  pops <- factor( rep( c("A","B"), each=4 ) )
  df <- data.frame(Population=pops,TPI=loci)
  
  fst <- Fst( df )
  
  expect_that( fst, is_a("data.frame"))
  expect_that( fst$Fst, is.nan )
  
  loci <- c( locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(2,2) ) )
  
  df$PGM <- loci
  
  fst <- Fst( df )
  expect_that( fst$Fst, equals(c(1.0, NaN)) )
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
  df <- data.frame(Population,locus)
  fst <- Fst( df )
  
  expect_that( fst$Hs, equals(0.475) )
  expect_that( fst$Ht, equals(0.5175) )
  
  locus2 <- c(AA,AA,AA,AA,AA,BB,BB,BB,BB,BB)
  df <- data.frame( Population, TPI=locus, PGM=locus2 )
  fst <- Fst( df )
  
  expect_that( fst$Hs, equals( c(0, 0.475)))
  expect_that( fst$Ht, equals( c(0.5, 0.5175)))

})

test_that("permutation P is a right-tail, strictly-positive p-value", {
  set.seed(1)
  AA <- locus( c("A","A") )
  BB <- locus( c("B","B") )
  AB <- locus( c("A","B") )

  # Strong structure (pops fixed for alternate alleles) -> Fst = 1 -> P small.
  strong <- data.frame(
    Population = rep(c("A","B"), each = 8),
    L = c(rep(AA, 8), rep(BB, 8))
  )
  fs <- Fst(strong, nperm = 99)
  expect_true(all(fs$P > 0))                 # add-one => never exactly zero
  expect_true(all(fs$P <= 1))
  expect_true(fs$P[1] < 0.2)                  # strong structure => clearly significant

  # No structure (identical allele makeup in both pops) -> Fst ~ 0 -> P large.
  none <- data.frame(
    Population = rep(c("A","B"), each = 8),
    L = c(rep(AA, 4), rep(BB, 4), rep(AA, 4), rep(BB, 4))
  )
  fn <- Fst(none, nperm = 99)
  expect_true(all(fn$P > fs$P))              # null is far less significant

  # Multiple loci where column order differs from alphabetical order
  set.seed(42)
  multi <- data.frame(
    Population = rep(c("A","B"), each = 10),
    Z_loc = c(rep(AA, 10), rep(BB, 10)),   # Fst = 1
    A_loc = c(rep(AA, 5), rep(BB, 5), rep(AA, 5), rep(BB, 5)) # Fst ~ 0
  )
  f_multi <- Fst(multi, nperm = 99)
  # Z_loc has strong differentiation, A_loc has none
  z_row <- f_multi[f_multi$Locus == "Z_loc", ]
  a_row <- f_multi[f_multi$Locus == "A_loc", ]
  expect_equal(z_row$Fst, 1.0)
  expect_true(z_row$P < 0.2)
  expect_true(a_row$P > 0.5)
})
