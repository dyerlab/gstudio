context("read_population.R")
rm(list=ls())

test_that("reading two-column csv file", {
  path <- system.file("extdata","data_2_column.csv",package="gstudio")
  
  # normal data.frame w/o genetic dta
  expect_that( data <- read.population(path), throws_error() )
   
  # bad locus.columns
  expect_that( (data <- read.population(path, locus.columns="BOB")), throws_error() )
   
  # real load with self correcting
  data <- read.population( path, locus.columns=4:7, is.two.column=TRUE)
  expect_that( data, is_a("data.frame") )
  expect_that( length(column_class(data,"locus")), equals(2) )
  
  # real load without self correcting
  data <- NULL
  data <- read.population( path, locus.columns=seq(4,7,by=2), is.two.column=TRUE)
  expect_that( data, is_a("data.frame") )
  expect_that( length(column_class(data,"locus")), equals(2) )
  
})


test_that("reading aflp data file",{
  path <- system.file("extdata","data_aflp.csv",package="gstudio")
  data <- read.population(path,locus.columns=4:7, is.aflp=TRUE)
  expect_that( data, is_a("data.frame") )
  expect_that( length( column_class(data,"locus")), equals(4) )
})


test_that("reading separated data file",{
  path <- system.file("extdata","data_separated.csv",package="gstudio")
  data <- read.population(path,locus.columns=4:5, is.separated=TRUE )
  print(data)
  expect_that( data, is_a("data.frame") )
  expect_that( length( column_class(data,"locus")), equals(4) )
})



test_that("reading snp data file",{
  path <- system.file("extdata","data_snp.csv",package="gstudio")
  data <- read.population(path,locus.columns=4:7, is.snp.minor=TRUE )
  expect_that( data, is_a("data.frame") )
  expect_that( length( column_class(data,"locus")), equals(4) )
})


test_that("reading zyme data file",{
  path <- system.file("extdata","data_zymelike.csv",package="gstudio")

#  data <- read.population(path,locus.columns=4:7, is.zyme=TRUE )
#  expect_that( data, is_a("data.frame") )
#  expect_that( length( column_class(data,"locus")), equals(4) )
})
