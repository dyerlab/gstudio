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


  expect_that( is_heterozygote(A), 		  is_false() )
  expect_that( is_heterozygote(AA), 		is_false() )
  expect_that( is_heterozygote(AB), 		is_true()  )
  expect_that( is_heterozygote(ZZ), 		is_false() )
  expect_that( is_heterozygote(AAAA), 	is_false() )
  expect_that( is_heterozygote(ABBB), 	is_true()  )
  expect_that( is_heterozygote(ABCD), 	is_true()  )
  expect_that( is_heterozygote(loci), 	is_equivalent_to( c(F, F, T, F, F, T, T)))
  
})

