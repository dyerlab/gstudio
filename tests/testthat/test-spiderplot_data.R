test_that("spiderplot_data input validation and coordinate mapping", {
  expect_error(spiderplot_data("not_df", data.frame()))
  expect_error(spiderplot_data(data.frame(a = 1), data.frame(b = 2)))

  pat <- data.frame(MomID = 1, OffID = 101, DadID = 2, Fij = 1.0)
  df <- data.frame(
    ID = c(1, 2, 101),
    OffID = c(0, 0, 101),
    Longitude = c(-110, -111, -110.5),
    Latitude = c(25, 26, 25.5)
  )

  sp <- spiderplot_data(pat, df)
  expect_s3_class(sp, "data.frame")
  expect_equal(sp$X, -110)
  expect_equal(sp$Xend, -111)
  expect_equal(sp$Y, 25)
  expect_equal(sp$Yend, 26)
})
