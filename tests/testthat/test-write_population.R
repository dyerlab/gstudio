
test_that("write_population error checks", {
  expect_error( write_population() )
  expect_error( write_population(FALSE) )
  expect_error( write_population(data.frame()) )
  expect_error( write_population(data.frame(x = 1), file = "tmp.txt", mode = "genepop") ) # missing stratum
})

test_that("write_population writes valid files across modes", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  df <- data.frame(
    Population = c(rep("P1", 4), rep("P2", 4)),
    TPI = c(rep(AA, 2), rep(AB, 2), rep(BB, 4)),
    PGM = c(rep(AB, 4), rep(AA, 4))
  )

  # text mode
  tmp_text <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp_text), add = TRUE)
  write_population(df, file = tmp_text, mode = "text")
  expect_true(file.exists(tmp_text))
  expect_true(file.info(tmp_text)$size > 0)

  # genepop mode
  tmp_gp <- tempfile(fileext = ".gen")
  on.exit(unlink(tmp_gp), add = TRUE)
  write_population(df, file = tmp_gp, mode = "genepop", stratum = "Population")
  expect_true(file.exists(tmp_gp))
  expect_true(file.info(tmp_gp)$size > 0)

  # structure mode
  tmp_st <- tempfile(fileext = ".str")
  on.exit(unlink(tmp_st), add = TRUE)
  write_population(df, file = tmp_st, mode = "structure", stratum = "Population")
  expect_true(file.exists(tmp_st))
  expect_true(file.info(tmp_st)$size > 0)

  # dfdist mode
  tmp_dfd <- tempfile(fileext = ".dfd")
  on.exit(unlink(tmp_dfd), add = TRUE)
  write_population(df, file = tmp_dfd, mode = "dfdist", stratum = "Population")
  expect_true(file.exists(tmp_dfd))
  expect_true(file.info(tmp_dfd)$size > 0)
})

