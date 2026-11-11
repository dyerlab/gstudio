test_that("hwe.test input validation", {
  expect_error( hwe.test("not_data_frame") )
  expect_error( hwe.test(data.frame(x = 1:5)) )
})

test_that("hwe.test on equilibrium data returns an htest", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))

  # Perfect HWE: p = 0.5, q = 0.5, N = 100
  loc <- c(rep(AA, 25), rep(AB, 50), rep(BB, 25))
  df <- data.frame(TPI = loc)

  res <- hwe.test(df, suppress_warnings = TRUE)
  expect_s3_class(res, "htest")
  expect_equal(unname(res$parameter), 1)
  expect_equal(names(res$statistic), "X-squared")
  expect_true(res$statistic < 0.01)
  expect_true(res$p.value > 0.95)
  expect_equal(res$data.name, "df$TPI")
  expect_equal(sum(res$observed), 100)

  res2 <- hwe.test(loc, suppress_warnings = TRUE)
  expect_s3_class(res2, "htest")
  expect_equal(res2$data.name, "loc")
  expect_equal(res2$statistic, res$statistic)
})

test_that("hwe.test with several loci or strata returns a table", {
  data(arapat)
  res <- suppressWarnings( hwe.test(arapat) )
  expect_s3_class(res, "data.frame")
  expect_named(res, c("Locus", "statistic", "df", "p.value", "method"))
  expect_equal(res$Locus, column_class(arapat, "locus"))
  expect_equal(res$statistic[res$Locus == "LTRS"],
               unname(hwe.test(arapat$LTRS, suppress_warnings = TRUE)$statistic))

  rs <- hwe.test(arapat, stratum = "Species", loci = c("LTRS", "WNT"), suppress_warnings = TRUE)
  expect_named(rs, c("Stratum", "Locus", "statistic", "df", "p.value", "method"))
  expect_equal(nrow(rs), 6)
  gr <- hwe.test(dplyr::group_by(arapat, Species), loci = c("LTRS", "WNT"), suppress_warnings = TRUE)
  expect_equal(gr, rs)
})

test_that("hwe.test small-sample warnings and continuity correction", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  loc <- c(rep(AA, 5), rep(AB, 5))
  df <- data.frame(L = loc)

  expect_warning(hwe.test(df))
  r <- hwe.test(df, suppress_warnings = TRUE)
  expect_match(r$method, "continuity correction")
})
