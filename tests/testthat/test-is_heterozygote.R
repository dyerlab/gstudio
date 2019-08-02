context("is_heterozygote.R")

test_that("general", {
  A <- locus( "A")
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  ZZ <- locus(  )
  AAAA <- locus( rep("A",4) )
  ABBB <- locus( c("A","B","B","B") )
  ABCD <- locus( LETTERS[1:4] )
  loci <- c(A, AA, AB, ZZ, AAAA, ABBB, ABCD )


  expect_false( is_heterozygote(A) )
  expect_false( is_heterozygote(AA) )
  expect_true( is_heterozygote(AB)  )
  expect_false( is_heterozygote(ZZ) )
  expect_false( is_heterozygote(AAAA) )
  expect_true( is_heterozygote(ABBB)  )
  expect_true( is_heterozygote(ABCD)  )
  expect_that( is_heterozygote(loci), 	is_equivalent_to( c(F, F, T, F, F, T, T)))
  
})

