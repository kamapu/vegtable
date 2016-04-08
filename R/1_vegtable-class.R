# TODO:   Set S4 object class "VegTable"
# 
# Author: Miguel Alvarez
################################################################################

setClass("vegtable", slots=c(
				description="character",
				samples="data.frame",
				head="data.frame",
				species="ANY",
				popups="list",
				coverconvert="ANY",
                syntax="ANY",
				log="list"),
		prototype=list(description=character(), samples=data.frame(),
				head=data.frame(), species=NULL, popups=list(),
				coverconvert=NULL, syntax=NULL, log=list()),
		validity=function(object) {
			if(!is.character(object@description))
				return("slot 'description' should be a character vector")
			if(!is.data.frame(object@samples))
				return("slot 'samples' should be a data frame")
			if(!is.data.frame(object@head))
				return("slot 'head' should be a data frame")
			if(!is.list(object@popups))
				return("slot 'popups' should be a list")
			if(!is.list(object@log))
				return("slot 'log' should be a list")
        }
)
