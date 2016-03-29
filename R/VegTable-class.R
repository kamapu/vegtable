# TODO:   Set S4 object class "VegTable"
# 
# Author: Miguel Alvarez
################################################################################

setClass("VegTable", slots=c(
				description="list",
				samples="data.frame",
				head="data.frame",
				species="data.frame",
				popups="list",
				coverconvert="list",
				log="list"),
		prototype=list(description=list(), samples=data.frame(),
				head=data.frame(), species=data.frame(), popups=list(),
				coverconvert=list(), log=list()),
		validity=function(object) {
			if(!is.list(object@description))
				return("slot description should be a list")
			if(!is.data.frame(object@samples))
				return("slot samples should be a data frame")
			if(!is.data.frame(object@head))
				return("slot head should be a data frame")
			if(!is.data.frame(object@species))
				return("slot species should be a data frame")
			if(!is.list(object@popups))
				return("slot popups should be a list")
			if(!is.list(object@coverconvert))
				return("slot coverconvert should be list")
			if(!is.list(object@log))
				return("slot log should be a list")
			return(TRUE)
		}
)
