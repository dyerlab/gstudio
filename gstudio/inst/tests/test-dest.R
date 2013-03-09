# context("dest.R")
# 
# test_that("checking",{
#   
#   loci.fixed <- c( locus( c(1,1) ),
#                    locus( c(1,1) ),
#                    locus( c(1,1) ),
#                    locus( c(1,1) ),
#                    locus( c(1,1) ),
#                    locus( c(1,1) ),
#                    locus( c(1,1) ),
#                    locus( c(1,1) ) )
#   
#   
#   pops <- factor( rep( c("A","B"), each=4 ) )
#   
#   est <- dest( pops, loci.fixed )
#   expect_that( est, is_a("structure_statistic") )
#   expect_that( est$estimate, equals(0) )
#   
#   
#   loci.fixed <- c( locus( c(1,1) ),
#                    locus( c(1,1) ),
#                    locus( c(1,1) ),
#                    locus( c(1,1) ),
#                    locus( c(2,2) ),
#                    locus( c(2,2) ),
#                    locus( c(2,2) ),
#                    locus( c(2,2) ) )
#   
#   est <- dest( pops, loci.fixed, size.correct=FALSE )
#   expect_that( est$estimate, equals(0.25) )
#   
#   loci <- c( locus( c(1,1) ),
#              locus( c(1,2) ),
#              locus( c(1,2) ),
#              locus( c(1,1) ),
#              locus( c(2,2) ),
#              locus( c(2,1) ),
#              locus( c(2,1) ),
#              locus( c(2,2) ) )
#   
#   est <- dest( pops, loci, size.correct=FALSE )
#   expect_that( est$estimate, equals( ((0.5-0.375) / (1-0.375) / 2 ) ) )
#   
#   loci <- c( locus( c(1,1) ),
#              locus( c(2,2) ),
#              locus( c(1,2) ),
#              locus( c(1,1) ),
#              locus( c(1,2) ),
#              locus( c(2,1) ),
#              locus( c(2,1) ),
#              locus( c(2,2) ) )
#   
#   est <- dest( pops, loci, size.correct=FALSE )
#   expect_that( est$estimate, equals( (0.5-0.46875) / (1-0.46875) / 2 ) )
#   
#   
#   
# })