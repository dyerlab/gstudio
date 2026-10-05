

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
  expect_true( all(c("Locus", "Gst", "Hs", "Ht", "p.value") %in% names(gs_gstp)) )

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
