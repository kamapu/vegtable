# TODO:   Working script for testing the package 'taxlist'
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
library(styler)
library(covr)

# Clean session
rm(list = ls())

# Clean folder
unlink(file.path("build-pkg", list.files("build-pkg", ".tar.gz")))
#unlink(file.path("build-pkg", list.files("build-pkg", ".pdf")))

# Write data
## source("data-raw/create-data.R")

# re-style scripts
style_pkg()

# write documentation
document()

# Build and check package
pkg_loc <- build(path = "build-pkg", args = "--resave-data")
check_built(path = pkg_loc)

# Report coverage
report()

# Install this version
install()

# Render the manual
build_manual(path = "build-pkg")
