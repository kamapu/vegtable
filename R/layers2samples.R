# TODO:   Function to move information from layers to samples
# 
# Author: Miguel Alvarez
################################################################################

setGeneric("layers2samples",
		function(object, layer, variable, ...)
			standardGeneric("layers2samples")
)

# Method for vegtable
setMethod("layers2samples", signature(object="vegtable", layer="character",
				variable="character"),
		function(object, layer, variable, ...) {
			if(!layer %in% names(object@layers))
				stop("Value of 'layer' is not occurring in slot 'layers'")
			if(any(!variable %in% colnames(object@layers[[layer]])))
				stop("Some values of 'variable' are not occurring in the targeted layer table")
			object@samples <- merge(object@samples, object@layers[[layer]][,
							unique(c(layer, variable))], by=layer, sort=FALSE,
					all.x=TRUE, all.y=FALSE)
			return(object)
		}
)

# Method for missing variables
setMethod("layers2samples", signature(object="vegtable", layer="character",
				variable="missing"),
		function(object, layer, variable, ...) {
			variable <- colnames(object@layers[[layer]])
			layers2samples(object, layer, variable)
		}
)
