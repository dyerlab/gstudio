
test_that("testing",{
  off  <- locus( 1:2 )
  mom  <- locus( c(1,1) )
  dad1 <- locus( c(2,2) )
  dad2 <- locus( c(1,2) )
  dad3 <- locus( c(1,1) )

  expect_that( transition_probability(), throws_error() ) 
  
  t1 <- transition_probability( off, mom, dad1 )
  t2 <- transition_probability( off, mom, dad2 )
  t3 <- transition_probability( off, mom, dad3 )
  
  expect_that( t1, equals(1.0) )
  expect_that( t2, equals(0.5) )
  expect_that( t3, equals(0.0) )

  
  t4 <- transition_probability( mom, dad3)  # 1.0
  t5 <- transition_probability( off, mom )  # 0.5
  t6 <- transition_probability( mom, dad1 ) # 0.0
  expect_that( t4, equals(1.0))
  expect_that( t5, equals(0.5))
  expect_that( t6, equals(0.0))
  
  
  # test to see if it handles 
  df <- data.frame( ID=1:5, PGM=c(off,mom,dad1,dad2,dad3))
  expect_that( transition_probability( df[1,], df[2,] ), is_equivalent_to(0.5))
  expect_that( transition_probability( df[2,], df[3,] ), is_equivalent_to(0.0))
  expect_that( transition_probability( df[2,], df[5,] ), is_equivalent_to(1.0))
  
  df$PGI <- df$PGM
  df$TPI <- df$PGM
  expect_that( transition_probability( df[1,], df[2,], df[4,] ), is_equivalent_to(0.5^3))
  expect_that( transition_probability( df[1,], df[2,], df[4,], multilocus=FALSE ), is_equivalent_to(c(0.5,0.5,0.5)))
  expect_that( transition_probability( df[1,], df[2,], df[3,] ), is_equivalent_to(1.0))
  expect_that( transition_probability( df[1,], df[2,], df[5,] ), is_equivalent_to(0.0))
  
  

  df$PGI[1] <- NA
  expect_that( transition_probability( df[1,], df[2,], df[4,], multilocus=FALSE ), is_equivalent_to( c(0.5,NA,0.5)) )
  expect_that( transition_probability( df[1,], df[2,], df[4,] ), is_equivalent_to( 0.25) )
  
               
})

test_that("one parent with allele frequencies gives P(off | parent, random mate)",{
  freqs <- data.frame( Allele = c("A","B","C"), Frequency = c(0.5, 0.3, 0.2) )
  T1 <- function( o, m ) transition_probability( locus(o), locus(m), freqs = freqs )

  expect_equal( T1( c("A","A"), c("A","A") ), 0.5 )               # p_A
  expect_equal( T1( c("A","B"), c("A","A") ), 0.3 )               # p_B
  expect_equal( T1( c("A","A"), c("A","B") ), 0.25 )              # p_A / 2
  expect_equal( T1( c("A","C"), c("A","B") ), 0.1 )               # p_C / 2
  expect_equal( T1( c("A","B"), c("A","B") ), (0.3 + 0.5) / 2 )   # (p_B + p_A) / 2
  expect_equal( T1( c("B","A"), c("A","B") ), (0.3 + 0.5) / 2 )
  expect_equal( T1( c("C","C"), c("A","B") ), 0 )                 # excluded
  expect_equal( T1( c("A","D"), c("A","B") ), 0 )                 # allele not in freqs

  # the ratio to a two-parent value is the paternity index
  pi <- transition_probability( locus(c("A","C")), locus(c("A","B")), locus(c("C","C")) ) /
    T1( c("A","C"), c("A","B") )
  expect_equal( pi, 0.5 / 0.1 )

  # without freqs the old compatibility score is unchanged
  expect_equal( transition_probability( locus(c("A","C")), locus(c("A","B")) ), 0.25 )

  # multilocus, with a frequencies() table
  df <- data.frame( ID = 1:2,
                    L1 = c( locus(c("A","C")), locus(c("A","B")) ),
                    L2 = c( locus(c("A","A")), locus(c("A","B")) ) )
  f <- frequencies( df )
  pA1 <- f$Frequency[ f$Locus == "L1" & f$Allele == "C" ]
  pA2 <- f$Frequency[ f$Locus == "L2" & f$Allele == "A" ]
  expect_equal( transition_probability( df[1,], df[2,], freqs = f, multilocus = FALSE ),
                c( L1 = pA1 / 2, L2 = pA2 / 2 ) )
  expect_equal( transition_probability( df[1,], df[2,], freqs = f ), pA1 / 2 * pA2 / 2 )

  expect_error( transition_probability( df[1,], df[2,], freqs = freqs ), "Locus" )
  expect_error( transition_probability( df$L1[1], df$L1[2], freqs = f ), "only that locus" )
})
