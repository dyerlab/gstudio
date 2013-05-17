#!/usr/bin/env bash

# This is a series of commands that I've found work together
#	to create a fully documented and ready R package.
# R.J. Dyer <rjdyer@vcu.edu>
#

perl -pi -e 's/\"}/}/g' ~/Documents/Dropbox/R/gstudio/gstudio/man/*.Rd
perl -pi -e 's/{\"/{/g' ~/Documents/Dropbox/R/gstudio/gstudio/man/*.Rd

