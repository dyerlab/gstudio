

test_that("individual",{

  expect_that( dist_bray("Bob"), throws_error() )
  expect_that( dist_bray(data.frame(Pop=1)), throws_error() )
  expect_that( dist_bray(data.frame(Pop=1), stratum="bob"), throws_error() )
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  AC <- locus( c("A","C") )
  BB <- locus( c("B","B") )
  BC <- locus( c("B","C") )
  CC <- locus( c("C","C") )
  
  
  loci <- c(AA,AA,AB,AA,BB,BC,CC,BB,BB,CC)
  df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )
  D <- dist_bray(df)
  expect_that( D, is_a("matrix") )
  expect_that( dim(D), is_equivalent_to(c(2,2)))
  expect_that( sum(diag(D)), equals(0) )
  expect_true( D[1,2]==D[2,1] )
  # Pop A: freq(A)=0.7, freq(B)=0.3. Pop B: freq(B)=0.5, freq(C)=0.5.
  # Shared alleles Ps = min(0.3, 0.5) = 0.3. Distance = 1 - 0.3 = 0.7.
  expect_equal( D[1,2], 0.7 )

  # Identical populations have distance 0
  df_ident <- data.frame(
    Population = c(rep("P1", 4), rep("P2", 4)),
    TPI = c(rep(AA, 2), rep(BB, 2), rep(AA, 2), rep(BB, 2))
  )
  expect_equal( dist_bray(df_ident)["P1", "P2"], 0 )

  # Disjoint populations have distance 1
  df_disj <- data.frame(
    Population = c(rep("P1", 4), rep("P2", 4)),
    TPI = c(rep(AA, 4), rep(CC, 4))
  )
  expect_equal( dist_bray(df_disj)["P1", "P2"], 1 )

  # Monomorphic locus does not crash to_mv_freq or dist_bray
  df_mono <- data.frame(
    Population = c(rep("P1", 3), rep("P2", 3)),
    TPI = rep(AA, 6)
  )
  expect_equal( dist_bray(df_mono)["P1", "P2"], 0 )
})