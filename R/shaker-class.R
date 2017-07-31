# TODO:   Class for Cocktail algorithms using vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

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
			if(any(duplicated(do.call(c, object@groups))))
				return("Some species groups are sharing taxon concepts")
			if(any(!c("TaxonConceptID","operator","value") %in%
							colnames(object@dominants)))
				return("Columns 'TaxonConceptID', 'operator', and 'value' are mandatory in slot 'dominants'")
			if(any(duplicated(apply(object@dominants, 1, format_F2))))
				return("Some rules in slot 'dominants' are duplicated")
			if(any(duplicated(do.call(c, object@formulas))))
				return("Some formulas are duplicated")
		}
)
