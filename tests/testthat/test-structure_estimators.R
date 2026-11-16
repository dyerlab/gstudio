# Estimator choices documented in ?genetic_structure.

AA <- locus(c("A","A")); AB <- locus(c("A","B")); BB <- locus(c("B","B"))
miss <- locus("")

test_that("Ht uses the unweighted mean of stratum frequencies", {
  # Pop P1: p(A) = 5/6 from 3 individuals; Pop P2: p(A) = 0 from 1 individual
  g <- Gst(c(AA, AA, AB, BB), c("P1","P1","P1","P2"), size.correct = FALSE)
  pbar <- mean(c(5/6, 0))
  expect_equal(g$Ht, 1 - pbar^2 - (1 - pbar)^2)        # not the pooled 5/8
  expect_equal(g$Hs, mean(c(1 - (5/6)^2 - (1/6)^2, 0)))
})

test_that("k and the harmonic mean size are counted per locus from genotyped individuals", {
  df <- data.frame(Population = c("P1","P1","P1","P2","P3","P3"),
                   L1 = c(AA, AB, AB, BB, miss, miss),   # P3 not genotyped at L1
                   L2 = c(AA, AB, AB, BB, AB, BB))
  r <- genetic_structure(df, mode = "Dest")

  # L1 uses only P1 and P2: k = 2, nh = harmonic mean of (3, 1)
  p <- c(2/3, 0)
  hs <- mean(1 - p^2 - (1 - p)^2); ht <- 1 - mean(p)^2 - (1 - mean(p))^2
  nh <- 1 / mean(1 / c(3, 1)); k <- 2
  hs <- 2 * nh / (2 * nh - 1) * hs
  ht <- ht + hs / (2 * k * nh)
  expect_equal(r$Dest[1], (ht - hs) / (1 - hs) * k / (k - 1))
  expect_equal(r$Hs[1], hs)
  expect_equal(r$Ht[1], ht)

  # A locus genotyped in only one stratum has no estimate
  one <- data.frame(Population = c("P1","P1","P2","P2"), L1 = c(AA, AB, miss, miss))
  expect_true(is.na(genetic_structure(one, mode = "Dest")$Dest))
})

test_that("multilocus = 'average' applies the estimator to mean Hs and Ht", {
  df <- data.frame(Population = rep(c("P1","P2"), each = 4),
                   L1 = c(AA,AB,AB,AA,BB,AB,AB,BB),
                   L2 = c(AA,AA,AB,AA,AB,BB,AB,BB))
  k <- 2
  for (m in c("Dest", "Gst_prime")) {
    h <- genetic_structure(df, mode = m)
    a <- genetic_structure(df, mode = m, multilocus = "average")
    expect_equal(h[1:2, ], a[1:2, ])                       # per-locus rows unchanged
    expect_equal(h[[m]][3], 1 / mean(1 / h[[m]][1:2]))      # harmonic mean
    hs <- a$Hs[3]; ht <- a$Ht[3]
    want <- if (m == "Dest") (ht - hs) / (1 - hs) * k / (k - 1) else
      (1 - hs / ht) * (k - 1 + hs) / ((k - 1) * (1 - hs))
    expect_equal(a[[m]][3], want)
  }
  # Gst is always the ratio of averages
  expect_equal(genetic_structure(df, multilocus = "average"), genetic_structure(df))
  expect_error(genetic_structure(df, mode = "Dest", multilocus = "geometric"))
})

test_that("Hs, Ht, Gst and Dest agree with mmod", {
  skip_on_cran()
  skip_if_not_installed("mmod")
  skip_if_not_installed("adegenet")
  data(arapat, package = "gstudio", envir = environment())
  loci <- column_class(arapat, "locus")
  m <- sapply(loci, function(l) { v <- as.character(arapat[[l]]); v[v == ""] <- NA; v })
  g <- suppressWarnings(adegenet::df2genind(m, sep = ":", ploidy = 2,
                                            pop = factor(arapat$Population)))
  ds <- mmod::diff_stats(g)
  dj <- mmod::D_Jost(g)

  gs <- genetic_structure(arapat, mode = "Gst")
  gd <- genetic_structure(arapat, mode = "Dest")
  per <- seq_along(loci)
  expect_equal(gs$Hs[per],  unname(ds$per.locus[, "Hs"]))
  expect_equal(gs$Ht[per],  unname(ds$per.locus[, "Ht"]))
  expect_equal(gs$Gst[per], unname(ds$per.locus[, "Gst"]))
  expect_equal(gd$Dest[per], unname(ds$per.locus[, "D"]))
  expect_equal(gs$Gst[length(loci) + 1], unname(ds$global["Gst_est"]))
  expect_equal(gd$Dest[length(loci) + 1], dj$global.harm_mean)
  expect_equal(genetic_structure(arapat, mode = "Dest", multilocus = "average")$Dest[length(loci) + 1],
               dj$global.het)

  # mmod's Gst_Hedrick() is Meirmans & Hedrick's (2011) G''st, a different estimator
  # (k per locus: populations with at least one genotype, as in both packages)
  k <- vapply(loci, function(l) length(unique(arapat$Population[as.character(arapat[[l]]) != ""])), numeric(1), USE.NAMES = FALSE)
  hs <- gs$Hs[per]; ht <- gs$Ht[per]
  expect_equal(k * (ht - hs) / ((k * ht - hs) * (1 - hs)),
               unname(mmod::Gst_Hedrick(g)$per.locus))
})
