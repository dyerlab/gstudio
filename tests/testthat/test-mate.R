
test_that( "testing",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  
  loci <- c(AA,AB) 
  momID <- c("A","B") 
  df <- data.frame( ID=factor(momID), TPI=loci )
  
  expect_that( mate(), throws_error())
  
  o <- mate(df[1,], df[1,], N=1)
  expect_that( o , is_a("data.frame") )
  expect_that( names(o), is_equivalent_to( c("ID","TPI")))
  expect_that( nrow(o), equals(1) )
  
  
  df$OffID=0
  o <- mate(df[1,], df[1,], N=10)
  expect_that( nrow(o), equals(10) )
  expect_that( names(o), is_equivalent_to( c("ID","OffID","TPI")))
  expect_that( as.character(o$ID[1]), equals("A"))
  expect_that( as.character(o$OffID[1]), equals("1"))
  
  # Parent with missing genotype produces offspring with missing genotype
  df_na <- df[1, ]
  df_na$TPI <- locus()
  o_na <- mate(df[1, ], df_na, N = 2)
  expect_equal(nrow(o_na), 2)
  expect_true(all(is.na(o_na$TPI)))
})
