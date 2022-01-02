# TODO:   Subset not working at the moment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install()

library(vegtable)

## source("R/clean.R")
## source("R/subset.R")

x = Kenya_veg
subset = substitute(ALTITUDE <= 1000)
slot = "header"
keep_children = FALSE
keep_parents = FALSE

Kenya_sub <- subset(x = Kenya_veg, subset = ALTITUDE <= 1000, slot = "header")
summary(Kenya_sub)
