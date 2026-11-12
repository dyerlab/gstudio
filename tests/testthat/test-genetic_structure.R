

test_that("checking",{
  loc1 <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  loc2 <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,3) ),
             locus( c(2,3) ) )
  pops <- rep( c("A","B"), each=2 )
  df <- data.frame( Population=pops, TPI=loc1 )
  
  gs <- genetic_structure( df  )
  expect_that( gs, is_a("data.frame") )
  expect_that( dim(gs), is_equivalent_to(c(1,5) ) )
  expect_that( names(gs), is_equivalent_to(c("Locus","Gst","Hs","Ht","p.value") ) )
  
  df$PGM <- loc2
  gs <- genetic_structure( df  )
  expect_that( gs, is_a("data.frame") )
  expect_that( dim(gs), is_equivalent_to(c(3,5) ) )
  expect_that( names(gs), is_equivalent_to(c("Locus","Gst","Hs","Ht","p.value") ) )
  expect_that( gs$Locus, is_equivalent_to( c("TPI","PGM","Multilocus")))
  expect_true( gs$Gst[1] < gs$Gst[2])
  
  gs <- genetic_structure( df, pairwise=TRUE )
  expect_that( gs, is_a("matrix") )
  expect_that( dim(gs), is_equivalent_to( c(2,2)) ) 
  expect_true( all( is.na(diag(gs))) ) 
  expect_true( gs[1,2] == gs[2,1] )

  # Test Fst mode
  gs_fst <- genetic_structure( df, mode = "fst" )
  expect_s3_class( gs_fst, "data.frame" )
  expect_true( all(c("Locus", "Hs", "Ht", "Fst") %in% names(gs_fst)) )

  # Test Gst_prime mode
  gs_gstp <- genetic_structure( df, mode = "gst_prime" )
  expect_s3_class( gs_gstp, "data.frame" )
  expect_true( all(c("Locus", "Gst_prime", "Hs", "Ht", "p.value") %in% names(gs_gstp)) )

  # Test Dest mode
  gs_dest <- genetic_structure( df, mode = "dest" )
  expect_s3_class( gs_dest, "data.frame" )
  expect_true( all(c("Locus", "Dest", "Hs", "Ht", "p.value") %in% names(gs_dest)) )

  # Test pairwise Dest
  gs_dest_pw <- genetic_structure( df, mode = "dest", pairwise = TRUE )
  expect_true( is.matrix(gs_dest_pw) )
  expect_equal( dim(gs_dest_pw), c(2, 2) )

  # Case-insensitivity
  gs_upper <- genetic_structure( df, mode = "FST" )
  expect_equal( gs_fst$Fst, gs_upper$Fst )
})
test_that("permutation gives the Multilocus row a p-value", {
  data(arapat, package = "gstudio", envir = environment())
  set.seed(1)
  gs <- genetic_structure(arapat, stratum = "Species", mode = "Gst", nperm = 19)
  ml <- gs[gs$Locus == "Multilocus", ]
  expect_false(is.na(ml$p.value))
  expect_true(ml$p.value >= 1 / 20 && ml$p.value <= 1)
  expect_false(anyNA(gs$p.value))
  expect_true(all(is.na(genetic_structure(arapat, stratum = "Species")$p.value)))
})

test_that("structure estimators: D_est scaling, naming, column order, small strata", {
  AA <- locus(c("A","A")); BB <- locus(c("B","B")); AB <- locus(c("A","B"))
  df <- data.frame(Population = rep(c("P1","P2"), each = 4),
                   L1 = c(AA,AA,AA,AA,BB,BB,BB,BB),
                   L2 = c(AA,AB,AB,AA,BB,AB,AB,BB),
                   L3 = c(AA,AA,AA,AA,AA,AA,AA,AA))   # monomorphic

  # Jost (2008): D = (Ht - Hs)/(1 - Hs) * k/(k-1); fixed differences give 1
  d <- genetic_structure(df, mode = "Dest", size.correct = FALSE)
  expect_equal(d$Dest[1], 1)
  expect_equal(d$Dest[2], (0.5 - 0.375) / (1 - 0.375) * 2)

  # Same column layout in every mode; estimate column named for the mode
  for (m in c("Gst", "Gst_prime", "Dest", "Fst")) {
    r <- suppressWarnings(genetic_structure(df, mode = m))
    expect_equal(names(r), c("Locus", m, "Hs", "Ht", "p.value"))
    expect_equal(r$Locus, c("L1", "L2", "L3", "Multilocus"))
    # A monomorphic locus has no estimate and does not zero the multilocus row
    expect_true(is.na(r[[m]][3]))
    expect_gt(r[[m]][4], 0)
    expect_gt(r$Hs[4], 0)
    expect_gt(r$Ht[4], 0)
  }

  # A stratum with a single heterozygote has Hs = 0.5, not 0
  g <- Gst(c(AB, AA, AB, BB), c("A","B","B","B"), size.correct = FALSE)
  expect_equal(g$Hs, 0.5)
  expect_equal(g$Gst, 0)

  # Permutations test every row, including Multilocus
  set.seed(2)
  p <- genetic_structure(df, mode = "Dest", nperm = 49)
  expect_false(anyNA(p$p.value[-3]))
  expect_true(is.na(p$p.value[3]))
})
