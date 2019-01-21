# TODO:   Methods for objects of class vegtable
# 
# Author: Miguel Alvarez
################################################################################

# Method for taxlist
setMethod("count_taxa", signature(object="vegtable"),
		function(object, level, include_lower=FALSE, ...) {
			concepts <- with(object@species@taxonNames,
					TaxonConceptID[match(object@samples$TaxonUsageID,
									TaxonUsageID)])
			if(!missing(level))
				if(!level %in% levels(object@species))
					stop("Value of argument 'level' is not a level in 'object'.")
			if(!missing(level) & include_lower) {
				concept_levels <- with(object@species@taxonRelations,
						as.integer(Level)[match(concepts, TaxonConceptID)])
				x <- which(levels(object@species) == level) - 1
				for(i in 1:x) {
					concepts[concept_levels == i] <-
							with(object@species@taxonRelations,
									Parent[match(concepts[concept_levels == i],
													TaxonConceptID)])
					concept_levels <- with(object@species@taxonRelations,
							as.integer(Level)[match(concepts, TaxonConceptID)])
				}
			}
			if(!missing(level)) {
				concept_levels <- with(object@species@taxonRelations,
						paste(Level)[match(concepts, TaxonConceptID)])
				concepts <- concepts[concept_levels == level]
			}
			return(length(unique(concepts)))
		}
)

# formula method
# Method for vegtable objects
setMethod("count_taxa", signature(object="formula"),
		function(object, data, include_lower=FALSE, ...) {
			nr_response <- attr(terms(object), "response")
			if(nr_response > 1)
				stop("More than one response in formula are not allowed.")
			if(nr_response == 1 & include_lower)
				data <- taxa2samples(data, as.character(object)[2]) else
				data <- taxa2samples(data)
			if(nr_response == 1) {
				concepts <- with(data@species@taxonNames,
						TaxonConceptID[match(data@samples$TaxonUsageID,
										TaxonUsageID)])
				concept_levels <- with(data@species@taxonRelations,
						as.integer(Level)[match(concepts, TaxonConceptID)])
				data@samples$TaxonUsageID[!concept_levels ==
								which(levels(data@species) ==
												as.character(object)[2])] <-
						NA
			}
			object <- as.formula(paste("TaxonUsageID ~",
							paste(attr(terms(object), "term.labels"),
									collapse=" + ")))
			if(all(is.na(data@samples$TaxonUsageID)))
				stop("No records for requested taxon rank.") else
				return(aggregate(object, data, function(x) length(unique(x)), ...))
			}
)
