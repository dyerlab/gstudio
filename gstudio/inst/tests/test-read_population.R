context("read_population.R")

test_that("reading two-column csv file", {
  path <- "~/Documents/Dropbox/R/gstudio/gstudio/data/lopho.raw.csv"
  locus_columns <- seq(3,62,2)
  
  # normal data.frame w/o genetic dta
  expect_that( data <- read_population(path), gives_warning() )
  
  # bad locus.columns
  expect_that( (data <- read_population(path, locus.columns="BOB")), throws_error() )
  
  # real load
  data <- read_population( path, locus.columns=seq(5,62,by=2), is.two.column=TRUE)
  expect_that( data, is_a("data.frame") )
  expect_that( length(column_class(data,"locus")), equals(29) )
})



test_that("reading aflp data file",{
  path <- "~/Documents/Dropbox/R/gstudio/gstudio/data/aflps.txt"
  
  expect_that(1,equals(1)) 
  
  data <- read_population(path,locus.columns=seq(2,141), sep=",", is.aflp=TRUE)
  
  expect_that( data, is_a("data.frame") )
  expect_that( length( column_class(data,"locus")), equals(140) )
  

})