test_that("exclusion_probability on single locus", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  loc <- c(rep(AA, 10), rep(AB, 20), rep(BB, 10))
  f <- frequencies(loc)
  
  res <- exclusion_probability(f)
  expect_s3_class(res, "data.frame")
  expect_true(res$Pexcl > 0 && res$Pexcl < 1)
  expect_true(res$PexclMax > 0 && res$PexclMax <= 1)
  expect_true(res$Fraction > 0 && res$Fraction <= 1)
})

test_that("exclusion_probability on multilocus and stratified data", {
  data(arapat)
  # Multilocus frequencies without stratum
  f_multi <- frequencies(arapat)
  res_multi <- exclusion_probability(f_multi)
  expect_s3_class(res_multi, "data.frame")
  expect_true(nrow(res_multi) > 1)
  expect_true(all(res_multi$Pexcl >= 0))

  # Stratified frequencies
  f_strat <- frequencies(arapat, stratum = "Population")
  res_strat <- exclusion_probability(f_strat)
  expect_s3_class(res_strat, "data.frame")
  expect_true("Stratum" %in% names(res_strat))
  expect_true(nrow(res_strat) > nrow(res_multi))
})

test_that("exclusion_probability error handling", {
  expect_error(exclusion_probability(data.frame(foo = 1:5)))
})
