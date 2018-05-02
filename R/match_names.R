# TODO:   Match character vectors with names in a taxlist object
# 
# Author: Miguel Alvarez
################################################################################

# Method for vegtable object
# Compare character vector with taxon names of taxlist
setMethod("match_names", signature(x="character", object="vegtable"),
		function(x, object, ...) {
			new_names <- match_names(x, object@species, ...)
			return(new_names)
		}
)
