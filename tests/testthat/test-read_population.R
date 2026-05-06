context("read_population.R")

test_that("error checking", {
  path <- system.file("extdata","data_2_column.csv",package="gstudio")
  # normal data.frame w/o genetic dta
  expect_that( data <- read.population(path), throws_error() )
  # bad locus.columns
  expect_that( (data <- read.population(path, locus.columns="BOB")), throws_error() )
  # wrong value for locus.columns
  expect_that( (data <- read.population(path, locus.columns=2:40)), throws_error() )
  # non-file, non text connection
  path=c(1,2,3)
  expect_that( (data <- read.population(path, locus.columns=2:40)), throws_error() )
})


test_that("two column file", {
  
  path <- system.file("extdata","data_2_column.csv",package="gstudio")

  # self-correcting locus.column
  data <- read_population( path, type="column", locus.columns=4:7)
  expect_that( data, is_a("data.frame"))
  expect_that( length(column_class(data,"locus")), equals(2) )
  
  # propertly specified locus.column
  data <- read_population( path, type="column", locus.columns=seq(4,7,by=2))
  expect_that( data, is_a("data.frame") )
  expect_that( length(column_class(data,"locus")), equals(2) )
})


test_that("reading aflp data file",{
  path <- system.file("extdata","data_aflp.csv",package="gstudio")
  data <- read_population(path, type="aflp", locus.columns=4:7)
  expect_that( data, is_a("data.frame") )
  expect_that( length( column_class(data,"locus")), equals(4) )
})


test_that("reading separated data file",{
  path <- system.file("extdata","data_separated.csv",package="gstudio")
  data <- read_population(path, type="separated", locus.columns=4:5, delim=":" )
  expect_that( data, is_a("data.frame") )
  expect_that( length( column_class(data,"locus")), equals(2) )
})


test_that("reading snp data file",{
  path <- system.file("extdata","data_snp.csv",package="gstudio")
  data <- read_population(path,type="snp",locus.columns=4:7 )
  expect_that( data, is_a("data.frame") )
  expect_that( length( column_class(data,"locus")), equals(4) )
})

test_that("reading zyme data file",{
  path <- system.file("extdata","data_zymelike.csv",package="gstudio")

  data <- read_population(path, type="zyme",locus.columns=4:7 )
  expect_that( data, is_a("data.frame") )
  expect_that( length( column_class(data,"locus")), equals(4) )
})


test_that("reading structure data file", {
  
  path <- system.file("extdata","data_structure.str", package="gstudio")
  data <- read_population(path, type="structure")
  expect_that( data, is_a("data.frame") )
  expect_that( length(column_class(data,"locus")), equals(15) )
  
})


test_that("reading snp data from textConnection",{
    df <- read.csv(system.file("extdata","data_snp.csv",package="gstudio"))
    vec <- c(paste0(paste(names(df),collapse=","),"\n"),sapply(1:nrow(df),function(l){paste0(paste(df[l,],collapse=", "),"\n")}))
    data <- read_population(path=textConnection(vec),type="snp",locus.columns=4:7,na.strings=c("NA"))
    expect_that( data, is_a("data.frame") )
    expect_that( length( column_class(data,"locus")), equals(4) )
})



