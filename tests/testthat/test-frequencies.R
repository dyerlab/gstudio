
test_that("frequencies.locus", {
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
  loc1 <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
  
  f <- frequencies( loc1 )
  expect_that( f, is_a("data.frame"))
  expect_that( names(f), is_equivalent_to(c("Allele","Frequency") ) )
  expect_that( unique(sort(alleles(loc1))), is_equivalent_to(sort(f$Allele) ) )
  expect_that( sum( f$Frequency ), equals(1))
    
})


test_that("frequencies.data.frame", {
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
  loc1 <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
  loc2 <- c(AA,AA,AC,AA,CC,CC,AC,CC,AA,AC)
  
  df <- data.frame( Population=c(rep("A",5),rep("B",5)), TPI=loc1, PGM=loc2 )
  f <- frequencies(df,loci="TPI")
  expect_that(f, is_a("data.frame"))
  expect_that(names(f), is_equivalent_to(c("Locus","Allele","Frequency")))
  
  f <- frequencies(df)
  expect_that( length(unique(f$Locus)), equals(2) )
  expect_that( sum( f$Frequency), equals(2) )
  
  expect_that( f <- frequencies( df, loci="bob"), throws_error() )
  expect_that( f <- frequencies( df, loci=c("bob","TPI")), throws_error() )
  expect_that( f <- frequencies( df, stratum="bob"), throws_error() )

})


test_that("frequencies with stratum returns correct columns and per-stratum sums", {
  AA <- locus(c("A","A")); AB <- locus(c("A","B")); BB <- locus(c("B","B"))
  df <- data.frame(Pop = c("A","A","B","B"), TPI = c(AA, AB, BB, AA))
  f <- frequencies(df, loci = "TPI", stratum = "Pop")

  expect_s3_class(f, "data.frame")
  expect_equal(names(f), c("Stratum", "Locus", "Allele", "Frequency"))
  expect_equal(sum(f$Frequency[f$Stratum == "A"]), 1)
  expect_equal(sum(f$Frequency[f$Stratum == "B"]), 1)
})


test_that("frequencies zero-fills missing allele/stratum combinations", {
  AA <- locus(c("A","A")); AB <- locus(c("A","B")); BB <- locus(c("B","B"))
  # Pop A has alleles A and B; Pop B has only A
  df <- data.frame(Pop = c("A","A","A","B","B"), TPI = c(AA, AB, BB, AA, AA))
  f <- frequencies(df, loci = "TPI", stratum = "Pop")

  # Every stratum must have a row for every allele
  expect_equal(nrow(f[f$Stratum == "A", ]), nrow(f[f$Stratum == "B", ]))
  expect_true(any(f$Stratum == "B" & f$Allele == "B" & f$Frequency == 0))
  # Filtering to observed alleles still sums to 1 per stratum
  expect_equal(sum(f$Frequency[f$Stratum == "B" & f$Frequency > 0]), 1)
})


test_that("frequencies (no stratum) is sorted by locus then allele", {
  AA <- locus(c("A","A")); BB <- locus(c("B","B")); CC <- locus(c("C","C"))
  # Column order TPI then PGM — output should reorder loci alphabetically
  df <- data.frame(TPI = c(BB, AA, CC), PGM = c(CC, AA, BB))
  f <- frequencies(df)

  expect_equal(unique(f$Locus), sort(unique(f$Locus)))
  for (loc in unique(f$Locus)) {
    al <- f$Allele[f$Locus == loc]
    expect_equal(al, sort(al))
  }
})


test_that("frequencies (with stratum) is sorted by stratum then locus then allele", {
  AA <- locus(c("A","A")); BB <- locus(c("B","B")); CC <- locus(c("C","C"))
  # Input rows in scrambled stratum order
  df <- data.frame(
    Pop = c("B","A","B","A"),
    TPI = c(BB, AA, AA, BB),
    PGM = c(CC, AA, CC, AA)
  )
  f <- frequencies(df, stratum = "Pop")

  expect_equal(f$Stratum, sort(f$Stratum))
  for (s in unique(f$Stratum)) {
    sub <- f[f$Stratum == s, ]
    expect_equal(sub$Locus, sort(sub$Locus))
    for (loc in unique(sub$Locus)) {
      al <- sub$Allele[sub$Locus == loc]
      expect_equal(al, sort(al))
    }
  }
})

# TODO: implement snp frequencies
# test_that("frequencies.data.frame", {
#   x <- matrix( abs( rnorm(30)), ncol=3)
#   x <- x / rowSums(x)
#   df <- data.frame( Loc1_1=x[,1], Loc1_2=x[,2], Loc1_3=x[,3])
#   
#   f <- frequencies(df)
#   expect_that( f, is_a("data.frame"))
#   expect_that( names(f), is_equivalent_to(c("Locus","Allele","Frequency")))
#   expect_that( f$Allele, is_equivalent_to(c("A","B")))
#   expect_that( f$Locus, is_equivalent_to(c("Loc1","Loc1")))
#   expect_that( sum(f$Frequency), is_equivalent_to(1))
# 
# })
# 




