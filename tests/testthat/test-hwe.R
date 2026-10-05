test_that("hwe input validation", {
  expect_error( hwe("not_data_frame") )
  expect_error( hwe(data.frame(x = 1:5)) )
})

test_that("hwe chi-square test on equilibrium data", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  
  # Perfect HWE: p = 0.5, q = 0.5, N = 100
  loc <- c(rep(AA, 25), rep(AB, 50), rep(BB, 25))
  df <- data.frame(TPI = loc)
  
  res <- hwe(df, supress_warnings = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(res$Locus, "TPI")
  expect_equal(res$df, 1)
  expect_named(res, c("Locus", "statistic", "df", "p.value"))
  expect_true(res$statistic < 0.01)
  expect_type(res$p.value, "double")
  expect_true(res$p.value > 0.95)
})

test_that("hwe works on locus vector input", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  loc <- c(rep(AA, 30), rep(AB, 40), rep(BB, 30))
  
  res <- hwe(loc, supress_warnings = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
})

test_that("hwe warnings on small sample size", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  loc <- c(rep(AA, 5), rep(AB, 5))
  df <- data.frame(L = loc)
  
  expect_warning(hwe(df, supress_warnings = FALSE))
})
