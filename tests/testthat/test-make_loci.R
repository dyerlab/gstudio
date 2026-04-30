context("make_loci.R")

test_that( "testing make_loci",{

  f <- data.frame( Allele=c(0,1,2), Frequency=c(0.2,0.5,0.3))
  loci <- make_loci( f )

  expect_is(loci,"locus")
  expect_equal(length(loci),20)

  loci <- make_loci( f, F=1.0 )
  expect_equal( sum(is_heterozygote(loci)), 0)

  # With F=0.5 the expected heterozygote count is 0.31 * 20 = 6.2.
  # rmultinom() gives correct stochastic draws, so verify the mean over many
  # replicates rather than a single deterministic value.
  set.seed(1)
  het_counts <- replicate(500, sum(is_heterozygote(make_loci(f, N=20, F=0.5))))
  expect_equal(mean(het_counts), 6.2, tolerance = 0.3)
})
