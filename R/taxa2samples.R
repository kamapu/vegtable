# TODO:   Add taxon IDs to slot samples
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("taxa2samples",
		function(object, ...)
			standardGeneric("taxa2samples")
)

# Method for vegtable objects
setMethod("taxa2samples", signature(object="vegtable"),
		function(object, merge_to, ...) {
			concepts <- with(object@species@taxonNames,
					TaxonConceptID[match(object@samples$TaxonUsageID,
									TaxonUsageID)])
			if(!missing(merge_to)) {
				if(!merge_to %in% levels(object@species))
					stop("Value of argument 'merge_to' is not a level in 'object'.")
				concept_levels <- with(object@species@taxonRelations,
						as.integer(Level)[match(concepts, TaxonConceptID)])
				x <- which(levels(object@species) == merge_to) - 1
				for(i in 1:x) {
					concepts[concept_levels == i & !is.na(concept_levels)] <-
							with(object@species@taxonRelations,
									Parent[match(concepts[concept_levels == i &
																	!is.na(concept_levels)],
													TaxonConceptID)])
					concept_levels <- with(object@species@taxonRelations,
							as.integer(Level)[match(concepts, TaxonConceptID)])
				}
			}
			object@samples$TaxonUsageID <- with(object@species@taxonRelations,
					AcceptedName[match(concepts, TaxonConceptID)])
			return(object)
		}
)
