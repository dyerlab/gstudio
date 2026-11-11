

test_that("error checks",{

  
  expect_that( genetic_diversity( data.frame(X=1), mode="Bob"), throws_error() )
  expect_that( genetic_diversity( numeric(10), mode="Ae"), throws_error() )
  expect_that( genetic_diversity( data.frame(A=numeric(0),mode="Ae")), throws_error() )
  
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  AD <- locus( c("A","D") )
  BC <- locus( c("B","C") )
  BD <- locus( c("B","D") )
  CC <- locus( c("C","C") )
  CD <- locus( c("C","D") )
  DD <- locus( c("D","D") )
  loci <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD) 
  stratum <- c(rep("A",3), rep("B",3), rep("C",4) )
  df <- data.frame( Population=stratum, Locus=loci)
  
  
  gd <- genetic_diversity( df )
  expect_that( gd, is_a("data.frame"))
  expect_that( dim(gd), is_equivalent_to(c(1,2)))
  expect_that( names(gd), is_equivalent_to(c("Locus","Ae")))
  expect_that( gd[1,2], is_equivalent_to( 4 ))
  
  gd <- genetic_diversity( df, stratum="Population")
  expect_that( dim(gd), is_equivalent_to(c(3,3)))
  expect_that( names(gd), is_equivalent_to(c("Stratum","Locus","Ae")))
    
})



test_that("all estimators are reached through genetic_diversity()", {
  data(arapat)

  # standalone estimators are no longer exported
  for( f in c("A","Ae","He","Hes","Hi","Ho","Hos","Ht","Fis","Pe") )
    expect_false( f %in% getNamespaceExports("gstudio") )

  # Hi: one row per individual, non-locus columns kept
  hi <- genetic_diversity( arapat, mode = "Hi" )
  expect_equal( nrow(hi), nrow(arapat) )
  expect_equal( names(hi), c(setdiff(names(arapat), column_class(arapat, "locus")), "Hi") )
  hi1 <- genetic_diversity( arapat, mode = "Hi", loci = "LTRS" )
  ok <- !is.na(arapat$LTRS)
  expect_equal( hi1$Hi[ok], as.numeric(is_heterozygote(arapat$LTRS))[ok] )

  # Hes, Hos and Ht need a stratum
  for( m in c("Hes","Hos","Ht") )
    expect_error( genetic_diversity( arapat, mode = m ), "stratum" )
  ht <- suppressWarnings( genetic_diversity( arapat, stratum = "Species", mode = "Ht" ) )
  expect_equal( names(ht), c("Locus","Ht") )
  expect_equal( nrow(ht), length(column_class(arapat, "locus")) )

  # loci= is honoured with a stratum too
  he <- genetic_diversity( arapat, stratum = "Species", mode = "He", loci = c("LTRS","WNT") )
  expect_setequal( unique(he$Locus), c("LTRS","WNT") )
  expect_error( genetic_diversity( arapat, mode = "He", loci = "Bob" ) )

  # a bare locus vector works
  fis <- genetic_diversity( arapat$LTRS, mode = "Fis" )
  expect_equal( names(fis), c("Locus","Fis") )
})

test_that("several modes give one joined table", {
  data(arapat)

  gd <- genetic_diversity( arapat, mode = c("A", "Ae", "He", "Ho") )
  expect_equal( names(gd), c("Locus", "A", "Ae", "He", "Ho") )
  expect_equal( nrow(gd), length(column_class(arapat, "locus")) )
  expect_equal( gd$He, genetic_diversity(arapat, mode = "He")$He )

  gs <- genetic_diversity( arapat, stratum = "Species", mode = c("Ae", "Fis") )
  expect_equal( names(gs), c("Stratum", "Locus", "Ae", "Fis") )
  expect_equal( gs$Fis, genetic_diversity(arapat, stratum = "Species", mode = "Fis")$Fis )

  # grouping supplies the stratum
  gg <- arapat |> dplyr::group_by(Species) |> genetic_diversity( mode = c("Ae", "Fis") )
  expect_equal( gg, gs )

  # pooled modes combine with each other; Ht has no Multilocus row
  gp <- suppressWarnings( genetic_diversity( arapat, stratum = "Species", mode = c("Hes", "Ht") ) )
  expect_equal( names(gp), c("Locus", "Hes", "Ht") )
  expect_true( is.na( gp$Ht[ gp$Locus == "Multilocus" ] ) )

  # duplicates dropped, case kept as typed
  expect_equal( names(genetic_diversity( arapat, mode = c("he", "He", "Ae") )), c("Locus", "he", "Ae") )

  expect_error( genetic_diversity( arapat, mode = c("Hi", "He") ), "Hi" )
  expect_error( genetic_diversity( arapat, stratum = "Species", mode = c("He", "Hes") ), "pooled" )
  expect_error( genetic_diversity( arapat, mode = c("He", "Bob") ) )
})

test_that("Dm mode", {
  loc1 <- c( locus(c(1,1)), locus(c(1,1)), locus(c(1,1)), locus(c(1,1)),
             locus(c(1,1)), locus(c(1,1)), locus(c(1,1)), locus(c(1,1)) )
  loc2 <- c( locus(c(1,1)), locus(c(1,1)), locus(c(1,2)), locus(c(1,1)),
             locus(c(2,2)), locus(c(2,2)), locus(c(2,2)), locus(c(1,2)) )
  df <- data.frame( Pop = rep(c("A","B"), each = 4), loc1, loc2 )

  d <- genetic_diversity( df, mode = "Dm" )
  expect_equal( names(d), c("Locus", "Dm") )
  expect_equal( d$Locus, "Multilocus" )
  expect_equal( d$Dm, 3/8 )

  # loci restriction
  expect_equal( genetic_diversity( df, loci = "loc1", mode = "Dm" )$Dm, 1/8 )

  # per stratum
  ds <- genetic_diversity( df, stratum = "Pop", mode = "Dm" )
  expect_equal( names(ds), c("Stratum", "Locus", "Dm") )
  expect_equal( ds$Dm, c(2/4, 2/4) )

  # combined with per-locus modes; Dm only on the Multilocus row
  dc <- genetic_diversity( df, mode = c("He", "Dm") )
  expect_equal( dc$Locus, c("loc1", "loc2", "Multilocus") )
  expect_true( all( is.na( dc$Dm[1:2] ) ) )
  expect_true( is.na( dc$He[3] ) )

  expect_error( genetic_diversity( df, stratum = "Pop", mode = c("Dm", "Hes") ), "pooled" )
})

test_that("per-locus modes are NA for a locus with no genotypes", {
  data(arapat)
  modes <- c("A", "Ae", "A95", "He", "Ho", "Fis", "Pe", "Sh", "Pex")
  # population 102 has no ZMP genotypes
  p <- arapat[arapat$Population == "102", ]
  g <- genetic_diversity(p, loci = c("ZMP", "LTRS"), mode = modes)
  expect_true(all(is.na(unlist(g[g$Locus == "ZMP", modes]))))
  expect_false(anyNA(unlist(g[g$Locus == "LTRS", modes])))
  expect_true(is.na(g$Pex[g$Locus == "Multilocus"]))

  gs <- suppressWarnings(genetic_diversity(arapat, stratum = "Population", loci = "ZMP", mode = "He"))
  expect_true(is.na(gs$He[gs$Stratum == "102"]))
  expect_equal(sum(is.na(gs$He)), 1)
})
