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
		function(object, merge_to, add_traits, ...) {
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
			object@samples$TaxonConceptID <- concepts
			object@samples$TaxonUsageID <- with(object@species@taxonRelations,
					AcceptedName[match(concepts, TaxonConceptID)])
			if(!missing(add_traits)) {
				if(!any(add_traits %in% colnames(object@species@taxonTraits)))
					stop("Values in 'add_traits' are not present as variable in the slot 'taxonTraits'.")
				if(any(!add_traits %in% colnames(object@species@taxonTraits)))
					warning("Some requested taxon traits are missing and will not appear in the output.")
				for(i in add_traits) {
					object@samples[,i] <- NA
					object@samples[,i] <- with(object@species@taxonTraits,
							replace_idx(object@samples[,i],
									object@samples$TaxonConceptID,
									TaxonConceptID, get(i)))
				}
			}
			return(object)
		}
)
