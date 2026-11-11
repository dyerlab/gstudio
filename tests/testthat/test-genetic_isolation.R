test_that("IBD is a Mantel test of genetic on spatial distance among strata", {
  data(arapat)
  set.seed(1)
  r <- genetic_isolation(arapat, nperm = 49)
  expect_s3_class(r, "htest")
  expect_equal(names(r$statistic), "r")

  G <- genetic_distance(arapat, mode = "Nei")
  D <- strata_distance(strata_coordinates(arapat), mode = "Circle")[rownames(G), colnames(G)]
  ok <- upper.tri(G)
  expect_equal(unname(r$statistic), cor(G[ok], D[ok]))
  expect_equal(unname(r$parameter["pairs"]), sum(ok))
  expect_true(r$p.value > 0 && r$p.value <= 1)

  # grouping supplies the stratum
  set.seed(1)
  rg <- genetic_isolation(dplyr::group_by(arapat, Species), nperm = 49)
  set.seed(1)
  rs <- genetic_isolation(arapat, stratum = "Species", nperm = 49)
  expect_equal(rg$statistic, rs$statistic)

  # a user-supplied spatial matrix wins
  rd <- genetic_isolation(arapat, spatial = D, nperm = 9)
  expect_equal(rd$statistic, r$statistic)
  expect_error(genetic_isolation(arapat, spatial = unname(D)), "names")
})

test_that("IBGD and PGD match ibgd() on the Population Graph", {
  data(gravity)
  pops <- unique(gravity$Population)
  deme <- setNames(as.integer(sub("Pop", "", pops)), pops)
  g <- population_graph(gravity)
  gx <- deme[igraph::V(g)$name]

  set.seed(2); a <- genetic_isolation(gravity, mode = "IBGD", axis = deme, nperm = 29)
  set.seed(2); b <- ibgd(g, x = gx, nperm = 29)
  expect_s3_class(a, c("ibgd", "htest"))
  expect_equal(a$statistic, b$statistic)
  expect_equal(a$p.value, b$p.value)

  p <- genetic_isolation(gravity, mode = "PGD", axis = deme)
  expect_equal(p$statistic, ibgd(g, x = gx, mode = "pgd")$statistic)

  # an explicit distance + forward matrix gives the same PGD
  D  <- abs(outer(deme, deme, "-"))
  Fw <- outer(deme, deme, function(i, j) j > i)
  expect_equal(genetic_isolation(gravity, mode = "PGD", spatial = D, forward = Fw)$statistic,
               p$statistic)

  # axis as a column name
  gravity$Deme <- deme[gravity$Population]
  expect_equal(genetic_isolation(gravity, mode = "PGD", axis = "Deme")$statistic, p$statistic)
})

test_that("genetic_isolation errors", {
  data(arapat); data(gravity)
  expect_error(genetic_isolation(arapat, mode = "bob"), "mode")
  expect_error(genetic_isolation(arapat, mode = "PGD"), "direction")
  expect_error(genetic_isolation(gravity), "Longitude")
  expect_error(genetic_isolation(arapat, stratum = "bob"), "stratum")
  expect_error(genetic_isolation(arapat, axis = c(a = 1)), "named")
  expect_error(genetic_isolation(data.frame(x = 1)), "locus")
})
