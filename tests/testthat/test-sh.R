test_that("Sh single locus", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  CD <- locus(c("C", "D"))
  expect_equal( Sh(c(AA, AA)), 0 )
  expect_equal( Sh(c(AB, AB)), log(2) )
  expect_equal( Sh(c(AB, CD)), log(4) )
  expect_error( Sh("bob") )
})

test_that("Sh through genetic_diversity", {
  data(arapat)
  s <- genetic_diversity( arapat, mode = "Sh" )
  expect_equal( names(s), c("Locus", "Sh") )
  expect_equal( s$Sh[ s$Locus == "LTRS" ], Sh(arapat$LTRS) )
  ss <- genetic_diversity( arapat, stratum = "Species", mode = c("Ae", "Sh") )
  expect_equal( names(ss), c("Stratum", "Locus", "Ae", "Sh") )
})
