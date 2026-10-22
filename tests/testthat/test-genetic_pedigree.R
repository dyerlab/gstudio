skip_if_not_installed("kinship2")

# Two founders, their son and daughter, and the child of that sib mating.
sib_mating <- function() {
  data.frame( ID = c("Sire", "Dam", "Son", "Daughter", "Child"),
              Sex = c("M", "F", "M", "F", "M"),
              Father = c(NA, NA, "Sire", "Sire", "Son"),
              Mother = c(NA, NA, "Dam", "Dam", "Daughter"),
              stringsAsFactors = FALSE )
}

test_that("inbreeding matches hand-calculated values", {
  F <- genetic_pedigree( sib_mating() )
  expect_type( F, "double" )
  expect_named( F, c("Sire", "Dam", "Son", "Daughter", "Child") )
  expect_equal( unname(F), c(0, 0, 0, 0, 0.25) )

  # Half sibs D and E share B; their child F has F = (1/2)^3 = 1/8
  half <- data.frame( ID = c("A", "B", "C", "D", "E", "F"),
                      Sex = c("F", "M", "F", "M", "F", "M"),
                      Father = c(NA, NA, NA, "B", "B", "D"),
                      Mother = c(NA, NA, NA, "A", "C", "E") )
  expect_equal( genetic_pedigree( half )[["F"]], 1/8 )

  # Parent-offspring mating: F = 1/4
  po <- data.frame( ID = c("S", "D", "X", "Y"), Sex = c("M", "F", "F", "M"),
                    Father = c(NA, NA, "S", "S"), Mother = c(NA, NA, "D", "X") )
  expect_equal( genetic_pedigree( po )[["Y"]], 0.25 )
})

test_that("an inbred common ancestor raises F by (1 + F_A)", {
  # B is the child of a sib mating (F_B = 1/4); his half-sib children D and
  # E produce G, so F_G = (1/2)^3 (1 + 1/4)
  ped <- data.frame(
    ID = c("P1", "P2", "S1", "S2", "B", "A", "C", "D", "E", "G"),
    Sex = c("M", "F", "M", "F", "M", "F", "F", "M", "F", "M"),
    Father = c(NA, NA, "P1", "P1", "S1", NA, NA, "B", "B", "D"),
    Mother = c(NA, NA, "P2", "P2", "S2", NA, NA, "A", "C", "E") )
  F <- genetic_pedigree( ped )
  expect_equal( F[["B"]], 0.25 )
  expect_equal( F[["G"]], (1/2)^3 * (1 + 0.25) )
})

test_that("table mode reports both paternities", {
  ped <- sib_mating()
  ped$AltFather <- c(NA, NA, NA, NA, "Sire")
  tab <- genetic_pedigree( ped, mode = "table" )
  expect_s3_class( tab, "data.frame" )
  expect_named( tab, c("ID", "Name", "F", "F_alt") )
  expect_equal( tab$ID, "Child" )
  # With Sire as father, Child is the offspring of a father-daughter mating
  expect_equal( tab$F_alt, 0.25 )
  expect_equal( genetic_pedigree( ped, father = "AltFather" )[["Child"]], 0.25 )

  all_rows <- genetic_pedigree( ped, mode = "table", inbred_only = FALSE )
  expect_equal( nrow(all_rows), 5 )
  expect_equal( all_rows$Name, all_rows$ID )
})

test_that("targaryen pedigree gives the expected coefficients", {
  data( targaryen, envir = environment() )
  expect_equal( dim(targaryen), c(101, 7) )
  F <- genetic_pedigree( targaryen )
  expect_equal( F[["Aenys_I"]], 0.25 )
  expect_equal( F[["Daeron_II"]], (1 + F[["Viserys_II"]]) / 8 + 1/8 )
  expect_equal( F[["Daenerys_S"]], 0.375 )
  expect_equal( F[["Viserys_I"]], 0.390625 )

  tab <- genetic_pedigree( targaryen, mode = "table" )
  jace <- tab[ tab$ID == "Jacaerys", ]
  expect_gt( jace$F, 0 )
  expect_equal( jace$F_alt, 0 )
  expect_equal( tab$F[ tab$ID == "Daeron_II" ], tab$F_alt[ tab$ID == "Daeron_II" ] )
})

test_that("kinship and relatedness matrices match expectations", {
  ped <- sib_mating()
  k <- genetic_pedigree( ped, mode = "kinship" )
  expect_true( is.matrix(k) )
  expect_equal( rownames(k), ped$ID )
  expect_equal( colnames(k), ped$ID )
  # Full sibs share theta = 1/4, which is F of their child
  expect_equal( k["Son", "Daughter"], 0.25 )
  expect_equal( k["Son", "Daughter"], genetic_pedigree( ped )[["Child"]] )
  expect_equal( k["Sire", "Dam"], 0 )

  r <- genetic_pedigree( ped, mode = "relatedness" )
  expect_equal( r, 2 * k )
  expect_equal( r["Sire", "Son"], 0.5 )
  # The diagonal is 1 + F
  expect_equal( unname( diag(r) ), 1 + unname( genetic_pedigree( ped ) ) )

  # Rows follow the input order, not kinship2's
  shuffled <- ped[ c(5, 3, 1, 4, 2), ]
  expect_equal( rownames( genetic_pedigree( shuffled, mode = "kinship" ) ), shuffled$ID )

  # Alternative paternity: Sire fathers Child with his daughter
  ped$AltFather <- c(NA, NA, NA, NA, "Sire")
  r_alt <- genetic_pedigree( ped, mode = "relatedness", father = "AltFather" )
  expect_equal( r_alt["Son", "Child"], 0.5 )
  expect_equal( r_alt["Sire", "Child"], 0.5 + 0.25 )
})

test_that("focal keeps the focal individuals and their ancestors", {
  data( targaryen, envir = environment() )
  boys <- c( "Jacaerys", "Aegon_III" )
  sub <- genetic_pedigree( targaryen, mode = "pedigree", focal = boys )
  expect_true( all( c( boys, "Rhaenyra", "Laenor", "Harwin_Strong",
                       "Daemon", "Aerion", "Valaena" ) %in% sub$id ) )
  expect_false( any( c( "Aegon_II", "Daenerys_S", "Lucerys" ) %in% sub$id ) )

  # Coefficients depend only on ancestors, so they are unchanged
  full <- genetic_pedigree( targaryen, mode = "relatedness" )
  part <- genetic_pedigree( targaryen, mode = "relatedness", focal = boys )
  expect_equal( part[ boys, boys ], full[ boys, boys ] )
  expect_equal( genetic_pedigree( targaryen, father = "AltFather", focal = boys )[["Jacaerys"]], 0 )

  expect_error( genetic_pedigree( targaryen, focal = "Nobody" ), "focal not found" )
})

test_that("pedigree and plot modes return kinship2 pedigrees", {
  p <- genetic_pedigree( sib_mating(), mode = "pedigree" )
  expect_s3_class( p, "pedigree" )

  f <- tempfile( fileext = ".pdf" )
  grDevices::pdf( f )
  ret <- genetic_pedigree( sib_mating(), mode = "plot" )
  grDevices::dev.off()
  unlink( f )
  expect_s3_class( ret, "pedigree" )
})

test_that("bad pedigrees are caught", {
  ped <- sib_mating()
  expect_error( genetic_pedigree( ped, mode = "Bob" ), "Unrecognized" )
  expect_error( genetic_pedigree( ped, mode = c("table", "plot") ), "single mode" )
  expect_error( genetic_pedigree( 1:5 ), "data.frame" )
  expect_error( genetic_pedigree( ped[, -2] ), "missing column" )
  expect_error( genetic_pedigree( ped, father = "AltFather" ), "not found" )

  bad <- ped; bad$ID[2] <- "Sire"
  expect_error( genetic_pedigree( bad ), "Duplicated" )
  bad <- ped; bad$Sex[3] <- "X"
  expect_error( genetic_pedigree( bad ), "Sex must be" )
  bad <- ped; bad$Mother[5] <- "Nobody"
  expect_error( genetic_pedigree( bad ), "not found in the ID column" )
  bad <- ped; bad$Mother[5] <- "Son"
  expect_error( genetic_pedigree( bad ), "must be female" )
  bad <- ped; bad$Mother[5] <- NA
  expect_error( genetic_pedigree( bad ), "both parents or neither" )

  # Empty strings are treated as missing parents
  ped$Father[1:2] <- ""; ped$Mother[1:2] <- ""
  expect_equal( genetic_pedigree( ped )[["Child"]], 0.25 )
})
