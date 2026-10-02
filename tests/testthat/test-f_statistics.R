
test_that("Inbreeding",{
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
  
  f <- Fis( loci )
  expect_that( f, is_equivalent_to(0.2))  
  
  data <- data.frame( A=loci, B=loci, C=loci)
  f <- Fis( data )
  expect_that( dim(f), is_equivalent_to( c(4,2) ) )
  expect_that( f, is_a("data.frame"))
  expect_that( f$Fis, is_equivalent_to( c(0.2,0.2,0.2,0.2)))

  # Stratified test with populations in non-alphabetical insertion order
  # Pop B has all heterozygotes (Ho = 1, He = 0.5, Fis = 1 - 1/0.5 = -1)
  # Pop A has all homozygotes (Ho = 0, He = 0.5, Fis = 1 - 0/0.5 = 1)
  df_strat <- data.frame(
    Population = c(rep("B", 10), rep("A", 10)),
    L1 = c(rep(AB, 10), rep(AA, 5), rep(BB, 5))
  )
  f_strat <- Fis(df_strat, stratum = "Population")
  expect_equal(f_strat$Fis[f_strat$Stratum == "B"], -1)
  expect_equal(f_strat$Fis[f_strat$Stratum == "A"], 1)
})