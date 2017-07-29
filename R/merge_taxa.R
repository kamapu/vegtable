# TODO:   A method of 'merge_taxa' for 'vegtable' objects.
# 
# Author: Miguel Alvarez
################################################################################

# Method for 'vegtable' object
setMethod("merge_taxa", signature(object="vegtable", concepts="numeric"),
		function(object, concepts, ...) {
			object@species <- merge_taxa(object@species, concepts, ...)
			return(object)
		}
)
