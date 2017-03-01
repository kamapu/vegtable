# TODO:   Set S4 object class "VegTable"
# 
# Author: Miguel Alvarez
################################################################################

setClass("vegtable", slots=c(
				description="character",
				samples="data.frame",
				head="data.frame",
				species="taxlist",
				relations="list",
				coverconvert="ANY"),
		prototype=list(
                description=character(),
                samples=data.frame(
                        ReleveID=integer(),
                        TaxonUsageID=integer()
                ),
				head=data.frame(
                        ReleveID=integer()
                ),
                species=new("taxlist"),
                relations=list(),
				coverconvert=NULL),
		validity=function(object) {
			if(!is.character(object@description))
				return("slot 'description' should be a character vector")
			if(!is.data.frame(object@samples))
				return("slot 'samples' should be a data frame")
			if(!is.data.frame(object@head))
				return("slot 'head' should be a data frame")
			if(!is.list(object@relations))
				return("slot 'relations' should be a list")
        }
)
