# TODO:   Class for Cocktail algorithms using vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# Function merging all elements of a formula into an 'index'
# Blanks will be deleted
# This function is used to detect duplicated formulas in shaker objects
format_F2 <- function(x) {
	x <- paste(x, collapse=" ")
	x <- gsub(" ", "", x, fixed=TRUE)
	x
}

setClass("shaker",
        slots=c(
                pseudos="list",
                groups="list",
                dominants="data.frame",
                formulas="list"),
        prototype=list(
                pseudos=list(),
                groups=list(),
                dominants=data.frame(TaxonConceptID=integer(),
                        operator=character(), value=numeric(),
                        stringsAsFactors=FALSE),
                formulas=list()),
        validity=function(object) {
			if(any(duplicated(do.call(c, object@pseudos))))
				return("Some pseudo-species are sharing taxon concepts")
			if(is.null(names(object@groups)))
				return("Members of slot 'groups' have to be named")
			if(any(duplicated(do.call(c, object@groups))))
				return("Some species groups are sharing taxon concepts")
			if(any(!c("TaxonConceptID","operator","value") %in%
							colnames(object@dominants)))
				return("Columns 'TaxonConceptID', 'operator', and 'value' are mandatory in slot 'dominants'")
			if(any(duplicated(apply(object@dominants, 1, format_F2))))
				return("Some rules in slot 'dominants' are duplicated")
			if(is.null(names(object@formulas)))
				return("Members of slot 'formulas' have to be named")
			if(any(duplicated(do.call(c, object@formulas))))
				return("Some formulas are duplicated")
		}
)
