context("bad_parents.R")

test_that("testing",{
  ID <- c(1:4,rep(1,4))
  OffID <- c(rep(0,4),1:4)
  PGM <- c( locus(c(1,2)),
            locus(c(1,1)),
            locus(c(2,2)),
            locus(c(1,2)),
            locus(c(1,1)),
            locus(c(2,2)),
            locus(c(1,2)),
            locus(c(3,3)))
  df <- data.frame( ID, OffID, PGM )
  p1 <- bad_parents(df)
  
  expect_that( p1, is_a("data.frame"))  
  expect_that(names(p1),is_equivalent_to(c("ID","OffID","PossibleParent")))
  expect_that(p1$ID, is_equivalent_to(as.character(rep(1,4))))
  expect_that(p1$OffID, is_equivalent_to(as.character(1:4)))
  expect_that(p1$PossibleParent, is_equivalent_to(c(T,T,T,F)))
  
  expect_that( bad_parents(),throws_error())
  expect_that( bad_parents(df[,1:2]), throws_error())
  expect_that( bad_parents(df,ID="Bob"), throws_error())
  expect_that( bad_parents(df,OffID="Bob"), throws_error())
  
})