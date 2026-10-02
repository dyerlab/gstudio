test_that("to_dfdist input validation and formatting", {
  expect_error(to_dfdist("not_df"))
  expect_error(to_dfdist(data.frame(x = 1:5))) # no loci
  expect_error(to_dfdist(data.frame(Pop = "A", L = locus(1:2)), stratum = "missing"))

  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  df <- data.frame(
    Population = c(rep("P1", 4), rep("P2", 4)),
    L1 = c(rep(AA, 2), rep(AB, 2), rep(BB, 4)),
    L2 = c(rep(AB, 4), rep(AA, 4))
  )

  txt <- to_dfdist(df)
  expect_true(is.character(txt))
  expect_true(nchar(txt) > 0)
  lines <- strsplit(txt, "\n")[[1]]
  expect_equal(lines[1], "0")
  expect_equal(lines[2], "2") # K = 2 strata
  expect_equal(lines[3], "2") # 2 loci
})
