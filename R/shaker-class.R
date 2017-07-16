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
            # No duplicated values
            # Mandatory names in 'dominants'
            if(any(!c("TaxonConceptID","operator","value") %in%
                            colnames(object@dominants)))
                return("Columns 'TaxonConceptID', 'operator', and 'value' are mandatory in slot 'dominants'")
        }
)
