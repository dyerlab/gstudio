# Rebuilds data/targaryen.rda: the pedigree of House Targaryen from George
# R.R. Martin's A Song of Ice and Fire, compiled by hand from Fire & Blood
# (2018) and The World of Ice & Fire (2014) and cross-checked against the
# father (P22) and mother (P25) statements in Wikidata (CC0).  The table is
# in data-raw/targaryen.csv (one row per individual); see ?targaryen.
#
# Run from the package root: source("data-raw/targaryen.R")

targaryen <- utils::read.csv( "data-raw/targaryen.csv",
                              na.strings = c("", "NA"),
                              stringsAsFactors = FALSE )

# Fails if the table is not a valid pedigree.
invisible( gstudio:::.validate_pedigree( targaryen ) )

save( targaryen, file = "data/targaryen.rda", compress = "xz" )
