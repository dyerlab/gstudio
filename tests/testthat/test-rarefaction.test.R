test_that("rarefaction.test input checks", {
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  loci <- c(AA,AB,AA,AA,AA,AA,AA,AA,AA,AA,AA)

  expect_error( rarefaction.test(loci), "size" )
  expect_error( rarefaction.test("bob", size = 2) )
  expect_error( rarefaction.test(loci, size = 5, mode = "Hes"), "modes" )
  expect_error( rarefaction.test(data.frame(L = loci, P = 1), stratum = "bob"), "stratum" )
  expect_warning( r <- rarefaction.test(loci, size = 20, nperm = 9), "Too few" )
  expect_true( is.na(r$estimate) )
})

test_that("observed values match genetic_diversity()", {
  data(arapat)
  modes <- c("A", "Ae", "A95", "He", "Ho", "Fis", "Pe", "Sh", "Pex")
  r <- rarefaction.test(arapat, stratum = "Species", mode = modes, nperm = 9)
  gd <- genetic_diversity(arapat, stratum = "Species", mode = modes)
  r <- r[r$Locus != "Multilocus", ]
  gd <- gd[gd$Locus != "Multilocus", ]
  for (m in modes) {
    a <- r[r$Mode == m, ]
    k <- match(paste(a$Stratum, a$Locus), paste(gd$Stratum, gd$Locus))
    expect_equal(a$observed, gd[[m]][k], tolerance = 1e-12, label = m)
  }
})

test_that("each locus is rarefied without replacement to the smallest stratum", {
  data(arapat)
  set.seed(1)
  r <- rarefaction.test(arapat, stratum = "Species", mode = c("A", "He"), nperm = 49)
  expect_s3_class(r, "rarefaction_test")
  expect_named(r, c("Stratum", "Locus", "Mode", "N", "size", "observed",
                    "estimate", "conf.low", "conf.high", "values"))
  n <- table(arapat$Species[!is.na(arapat$WNT)])
  w <- r[r$Locus == "WNT", ]
  expect_true(all(w$size == min(n)))
  expect_equal(w$N[w$Mode == "A"], as.numeric(n[w$Stratum[w$Mode == "A"]]))

  # the smallest stratum is used as observed
  small <- w[w$N == w$size, ]
  expect_equal(small$estimate, small$observed)
  expect_true(all(small$conf.low == small$conf.high))

  # without replacement, rarefied allele counts never exceed the observed
  a <- r[r$Mode == "A" & r$Locus != "Multilocus", ]
  expect_true(all(a$conf.high <= a$observed))
  expect_true(all(r$conf.low <= r$conf.high))

  # resampled values, one column per row
  expect_true(all(lengths(r$values) == 49))
  expect_equal(vapply(r$values, mean, numeric(1)), r$estimate)
  expect_output(print(r), "Rarefaction without replacement")
  expect_false(any(grepl("Stratum.*values", capture.output(print(r)))))

  # values stay with their rows through filtering
  f <- dplyr::filter(r, Mode == "He", Locus == "LTRS")
  expect_equal(vapply(f$values, mean, numeric(1)), f$estimate)
})

test_that("Multilocus rows combine resamples across loci", {
  data(arapat)
  set.seed(2)
  r <- rarefaction.test(arapat, stratum = "Species", loci = c("LTRS", "WNT"),
                        mode = c("Ae", "Fis", "Pex"), nperm = 19)
  v <- do.call(cbind, r$values)
  row <- function(l, m) which(r$Stratum == "Cape" & r$Locus == l & r$Mode == m)
  expect_equal(v[, row("Multilocus", "Ae")], (v[, row("LTRS", "Ae")] + v[, row("WNT", "Ae")]) / 2)
  expect_equal(v[, row("Multilocus", "Pex")],
               1 - (1 - v[, row("LTRS", "Pex")]) * (1 - v[, row("WNT", "Pex")]))
  cape <- genetic_diversity(arapat[arapat$Species == "Cape", ], loci = c("LTRS", "WNT"),
                            mode = c("Ho", "He"))
  expect_equal(r$observed[row("Multilocus", "Fis")], 1 - sum(cape$Ho) / sum(cape$He))
})

test_that("grouping, a single sample, and a fixed size", {
  data(arapat)
  set.seed(3); a <- rarefaction.test(dplyr::group_by(arapat, Species), loci = "LTRS", nperm = 19)
  set.seed(3); b <- rarefaction.test(arapat, stratum = "Species", loci = "LTRS", nperm = 19)
  expect_equal(a$estimate, b$estimate)
  expect_equal(a$values, b$values)

  s <- rarefaction.test(arapat$LTRS, size = 20, nperm = 19)
  expect_false("Stratum" %in% names(s))
  expect_equal(s$size, 20)

  f <- suppressWarnings(rarefaction.test(arapat, stratum = "Species", loci = "LTRS",
                                         size = 50, nperm = 19))
  expect_true(is.na(f$estimate[f$Stratum == "Mainland"]))
  expect_warning(rarefaction.test(arapat, stratum = "Species", loci = "LTRS", size = 50, nperm = 9),
                 "Mainland LTRS")
})

test_that("na.rm controls how Multilocus treats loci missing in a stratum", {
  data(arapat)
  # population 102 has no ZMP genotypes
  set.seed(4)
  r <- suppressWarnings(rarefaction.test(arapat, loci = c("LTRS", "ZMP", "EN"),
                                         mode = c("Ae", "Pex"), nperm = 19))
  ml <- r[r$Stratum == "102" & r$Locus == "Multilocus", ]
  expect_true(all(is.na(ml$observed) & is.na(ml$estimate)))
  expect_false(anyNA(r$estimate[r$Stratum == "101" & r$Locus == "Multilocus"]))

  set.seed(4)
  rn <- suppressWarnings(rarefaction.test(arapat, loci = c("LTRS", "ZMP", "EN"),
                                          mode = c("Ae", "Pex"), nperm = 19, na.rm = TRUE))
  row <- function(l, m) which(rn$Stratum == "102" & rn$Locus == l & rn$Mode == m)
  expect_equal(rn$observed[row("Multilocus", "Ae")],
               mean(rn$observed[c(row("LTRS", "Ae"), row("EN", "Ae"))]))
  expect_equal(rn$values[[row("Multilocus", "Pex")]],
               1 - (1 - rn$values[[row("LTRS", "Pex")]]) * (1 - rn$values[[row("EN", "Pex")]]))
  # strata with every locus present are unchanged
  expect_equal(rn$estimate[rn$Stratum == "101"], r$estimate[r$Stratum == "101"])
})
