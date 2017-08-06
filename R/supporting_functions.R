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

# Function merging all elements of a formula into an 'index'
# Blanks will be deleted
# This function is used to detect duplicated formulas in shaker objects
format_F2 <- function(x) {
	x <- paste(x, collapse=" ")
	x <- gsub(" ", "", x, fixed=TRUE)
	x
}

# Function replacing syntax in shaker objects
# companion is a taxlist object
# Written for the summary output
rewrite_formulas <- function(shaker, companion) {
	EQ <- list()
	for(i in names(shaker@formulas)) EQ[[i]] <- {
		x <- shaker@formulas[[i]]
		if(grepl("\'", x)) SYM <- "\'"
		if(grepl('\"', x)) SYM <- '\"'
		x <- gsub("groups[[", "groups:", x, fixed=TRUE)
		x <- gsub("dominants[[", "species:", x, fixed=TRUE)
		x <- gsub("]]", "", x, fixed=TRUE)
		Spp <- as.numeric(unlist(regmatches(x,
								gregexpr("[[:digit:]]+\\.*[[:digit:]]*", x))))
		if(length(Spp) > 0){
			for(j in Spp) {
				subformula <- shaker@dominants[j,]
				subformula$TaxonConceptID <- companion[
						match(subformula$TaxonConceptID,
								companion$TaxonConceptID),"TaxonName"]
				subformula <- paste(subformula[1,], collapse=" ")
				
				x <- sub(paste0("species:", j), paste0("species:", SYM,
								subformula, SYM), x)
			}
		}
		x
	}
	return(EQ)
}
