#!/usr/bin/env bash

# This is a series of commands that I've found work together
#	to create a fully documented and ready R package.
# R.J. Dyer <rjdyer@vcu.edu>
#

# remove all the old documentation and other cruft
rm -rf ./gstudio/man/*.Rd
rm -rf ./gstudio/inst/doc/*.log
rm -rf ./gstudio/inst/doc/*.tex
rm -rf ./gstudio/inst/doc/*.pdf
rm -rf ./gstudio/inst/doc/*.gz
rm -rf ./gstudio/inst/doc/*.toc
rm -rf ./gstudio/inst/doc/*.html
rm -rf ./gstudio/inst/doc/*.md
rm -rf ./gstudio/inst/doc/figure/


# build the documentation using ROxygen
echo "require(roxygen2);roxygenize('./gstudio');q(save='no')" | R --vanilla --silent

# Correce the documentation for operator overloads
perl -pi -e 's/\"}/}/g' ./gstudio/man/*.Rd
perl -pi -e 's/{\"/{/g' ./gstudio/man/*.Rd

# build the package with compacted vignettes
R CMD build ./gstudio --compact-vignettes 

# check the package against CRAN
R CMD check gstudio_1.0.tar.gz --as-cran

