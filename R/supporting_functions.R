# TODO:   Hidden functions, not exported in namespace
# 
# Author: Miguel Alvarez
################################################################################

# Function merging names in splitted formulas
# Assumes the two last elements are 'operator' and 'value'
# Previous elements are then part of the species name
format_F1 <- function(x) {
	NR <- length(x)
	return(c(paste(x[1:(NR - 2)], collapse=" "), x[(NR - 1):NR]))
}

# Function merge all elements of a formula into an 'index'
# Blanks will be deleted
# This function is used to detect duplicated formulas in shaker objects
format_F2 <- function(x) {
	x <- paste(x, collapse=" ")
	x <- gsub(" ", "", x, fixed=TRUE)
	x
}
