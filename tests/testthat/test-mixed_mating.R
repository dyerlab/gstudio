
test_that( "testing",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  loci <- c(AA,AB) 
  momID <- c("A","B") 
  df <- data.frame( ID=factor(momID), TPI=loci )
  
  expect_that( mixed_mating(), throws_error())
  
  o <- mixed_mating(df, s=0)
  expect_that( o , is_a("data.frame") )
  expect_that( names(o), is_equivalent_to( c("ID","TPI")))
  expect_that( nrow(o), equals(2) )
  
  o <- mixed_mating(df[1,], s=1, N=10)
  expect_that( nrow(o), equals(10) )
  expect_that( names(o), is_equivalent_to( c("ID","TPI")))
  
  expect_true( sum( is_heterozygote(o$TPI))==0 )
  
})

test_that( "offspring are Mendelian and missing genotypes propagate", {
  set.seed( 1 )
  df <- data.frame( ID = 1:3,
                    L1 = c( locus( c( "12", "12" ) ),
                            locus( c( "9", "9" ) ),
                            locus() ) )
  o <- mixed_mating( df[1, ], N = 20, s = 1 )
  expect_true( all( as.character( o$L1 ) == "12:12" ) )

  # mother 12:12 x father 9:9 always gives the heterozygote, sorted as locus()
  pair <- .offspring_genotypes( rep( "12:12", 50 ), rep( "9:9", 50 ) )
  expect_s3_class( pair, "locus" )
  expect_true( all( as.character( pair ) == as.character( locus( c( "9", "12" ) ) ) ) )

  # either parent missing gives a missing offspring
  miss <- .offspring_genotypes( c( "", "A:B" ), c( "A:A", "" ) )
  expect_true( all( is.na( miss ) ) )
})

test_that( "segregation is 1:2:1 and ID / OffID are set", {
  set.seed( 2 )
  df <- data.frame( ID = 1, OffID = 0, L1 = locus( c( "A", "B" ) ) )
  o  <- mixed_mating( df, N = 4000, s = 1 )
  expect_equal( o$ID, seq_len( 4000 ) )
  expect_equal( o$OffID, seq_len( 4000 ) )
  tab <- table( as.character( o$L1 ) ) / 4000
  expect_equal( as.numeric( tab[c( "A:A", "A:B", "B:B" )] ),
                c( 0.25, 0.5, 0.25 ),
                tolerance = 0.03 )
})

test_that( "with s = 0 every adult is a mother once", {
  set.seed( 3 )
  df <- data.frame( Population = "P", ID = 1:10, Mom = 1:10 )
  df$L1 <- rep( locus( c( "A", "B" ) ), 10 )
  o  <- mixed_mating( df, N = 2 )
  expect_equal( nrow( o ), 20 )
  expect_equal( as.vector( table( o$Mom ) ), rep( 2L, 10 ) )
  expect_equal( names( o ), names( df ) )
})
