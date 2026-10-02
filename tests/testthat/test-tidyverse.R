test_that("dplyr::group_by works seamlessly with frequencies and population metrics", {
  library(dplyr)
  data(arapat)

  # 1. Piped frequencies with group_by(Population) matches explicit stratum
  res_pipe <- arapat |>
    select(Population, LTRS) |>
    group_by(Population) |>
    frequencies()

  res_explicit <- frequencies(arapat, stratum = "Population", loci = "LTRS")
  expect_s3_class(res_pipe, "data.frame")
  expect_equal(res_pipe$Stratum, res_explicit$Stratum)
  expect_equal(res_pipe$Allele, res_explicit$Allele)
  expect_equal(res_pipe$Frequency, res_explicit$Frequency)

  # 2. Grouping by a non-default column name (e.g., Cluster)
  arapat_clust <- arapat |>
    mutate(Cluster = substr(Population, 1, 1)) |>
    group_by(Cluster)

  res_clust <- arapat_clust |>
    select(Cluster, LTRS) |>
    frequencies()
  expect_true(all(res_clust$Stratum %in% unique(substr(arapat$Population, 1, 1))))

  # 3. genetic_diversity with grouped_df
  gd <- arapat |>
    select(Population, LTRS) |>
    group_by(Population) |>
    genetic_diversity(mode = "He")
  expect_s3_class(gd, "data.frame")
  expect_true("Stratum" %in% names(gd))

  # 4. Fst with grouped_df
  fst <- arapat |>
    select(Population, LTRS) |>
    group_by(Population) |>
    Fst()
  expect_s3_class(fst, "data.frame")
  expect_equal(fst$Locus, "LTRS")
  expect_true(!is.na(fst$Fst))

  # 5. Gst, Gst_prime, Dest with grouped_df
  gst <- arapat |>
    select(Population, LTRS) |>
    group_by(Population) |>
    Gst()
  expect_equal(gst$Locus, "LTRS")

  gstp <- arapat |>
    select(Population, LTRS) |>
    group_by(Population) |>
    Gst_prime()
  expect_equal(gstp$Locus, "LTRS")

  dest <- arapat |>
    select(Population, LTRS) |>
    group_by(Population) |>
    Dest()
  expect_equal(dest$Locus, "LTRS")

  # 6. Fis with grouped_df
  fis <- arapat |>
    select(Population, LTRS) |>
    group_by(Population) |>
    Fis()
  expect_true("Stratum" %in% names(fis))

  # 7. partition with grouped_df
  pt <- arapat |>
    group_by(Population) |>
    partition()
  expect_equal(length(pt), length(unique(arapat$Population)))
  # Each slice should be an ungrouped data.frame
  expect_false(inherits(pt[[1]], "grouped_df"))
})
