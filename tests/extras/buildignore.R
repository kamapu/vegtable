# TODO:   Examples inserting lines to .Rbuildignore
# 
# Author: Miguel Alvarez
################################################################################

require(devtools)

# Example for this file
use_build_ignore(file.path("tests/extras", c("buildignore.R","example_data.R")),
        pkg="M:/WorkspaceEclipse/vegtable")

# README files
use_build_ignore(c("README-figures","README.md"),
        pkg="M:/WorkspaceEclipse/vegtable")
