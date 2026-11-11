test_that("Pex single locus", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  loc <- c(rep(AA, 10), rep(AB, 20), rep(BB, 10))

  # two alleles at p = q = 0.5: 2(0.5)(0.25) - 0.5 * 2 * (0.0625)(1) = 0.1875
  expect_equal( Pex(loc), 0.1875 )
  expect_equal( Pex(c(AA, AA, AA)), 0 )
  expect_error( Pex("bob") )
})

test_that("Pex through genetic_diversity", {
  data(arapat)
  p <- genetic_diversity( arapat, mode = "Pex" )
  expect_equal( names(p), c("Locus", "Pex") )
  expect_equal( p$Locus, c(column_class(arapat, "locus"), "Multilocus") )
  per <- p$Pex[ p$Locus != "Multilocus" ]
  expect_true( all(per >= 0 & per < 1) )
  expect_equal( p$Pex[ p$Locus == "Multilocus" ], 1 - prod(1 - per) )

  # one locus: no Multilocus row
  expect_equal( nrow(genetic_diversity( arapat, loci = "LTRS", mode = "Pex" )), 1 )

  ps <- genetic_diversity( arapat, stratum = "Species", mode = "Pex" )
  expect_equal( names(ps), c("Stratum", "Locus", "Pex") )
  expect_equal( sum(ps$Locus == "Multilocus"), 3 )
})
