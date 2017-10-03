# TODO:   A method of 'merge_taxa' for 'vegtable' objects.
# 
# Author: Miguel Alvarez
################################################################################

# Method for 'vegtable' object
setMethod("merge_taxa", signature(object="vegtable", concepts="numeric",
				level="missing"),
		function(object, concepts, level, ...) {
			object@species <- merge_taxa(object@species, concepts, ...)
			return(object)
		}
)

setMethod("merge_taxa", signature(object="vegtable", concepts="missing",
				level="character"),
		function(object, concepts, level, ...) {
			object@species <- merge_taxa(object@species, level=level, ...)
			return(object)
		}
)



