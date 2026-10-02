

test_that("individual",{

  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  AC <- locus( c("A","C") )
  BB <- locus( c("B","B") )
  BC <- locus( c("B","C") )
  CC <- locus( c("C","C") )
  loci <- c(AA,AA,AB,AA,BB,BC,CC,BB,BB,CC)
  df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )

  
  expect_that( dist_jaccard("Bob"), throws_error() )
  expect_that( dist_jaccard(data.frame(Population="A")), throws_error() )
  
  D <- dist_jaccard(df)
  expect_that( D, is_a("matrix") )
  expect_that( dim(D), is_equivalent_to(c(2,2)))
  expect_that( sum(diag(D)), equals(0) )
  expect_true( D[1,2]==D[2,1] )
  # J = 2B / (1 + B) where B = 0.7
  expect_equal( D[1,2], (2 * 0.7) / (1 + 0.7) )

  # Identical populations have Jaccard distance 0
  df_ident <- data.frame(
    Population = c(rep("P1", 4), rep("P2", 4)),
    TPI = c(rep(AA, 2), rep(BB, 2), rep(AA, 2), rep(BB, 2))
  )
  expect_equal( dist_jaccard(df_ident)["P1", "P2"], 0 )

  # Disjoint populations have Jaccard distance 1
  df_disj <- data.frame(
    Population = c(rep("P1", 4), rep("P2", 4)),
    TPI = c(rep(AA, 4), rep(CC, 4))
  )
  expect_equal( dist_jaccard(df_disj)["P1", "P2"], 1 )
})