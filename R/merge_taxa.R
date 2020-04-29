#' @name merge_taxa
#' 
#' @title Merge concepts
#' 
#' @description 
#' Merge taxon concepts form into single ones or insert accepted names to slot
#' samples.
#' 
<<<<<<< HEAD
#' This method is applied to a function defined in the package [taxlist] and
#' only modify the slot `species` in the input `object`.
=======
#' This method is applied to a function defined in the package [taxlist-package]
#' and only modify the slot `species` in the input `object`.
>>>>>>> refs/remotes/origin/miguel
#' 
#' The use of `taxa2samples()` with `merge_to` argument will produce a similar
#' result as using `merge_taxa` with `level` argument, but `taxa2samples()`
#' will replace the records in slot samples by the respective accepted names
#' without any modification in slot species.
#' Additionally taxon concept IDs will be addes as columns in samples and taxon
#' traits if indicated in argument `add_traits`.
#' 
#' @param object Object of class [vegtable-class].
#' @param concepts Numeric (integer) vector including taxon concepts to be
#'     merged.
#' @param level,merge_to Character value indicating the level to which the taxa
#'     have to be merged.
#' @param add_traits A character vector indicating variables in the slot
#'     `taxonTraits` to be added in slot `samples`.
#' @param ... Further arguments passed to [taxlist::merge_taxa()].
#' 
#' @return An object of class [vegtable-class].
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples
#' ## Merge Olea capensis into one
#' summary(subset(Kenya_veg@species, grepl("Olea capensis", TaxonName),
#' 	   slot="names"), "all")
#' Kenya_veg <- merge_taxa(Kenya_veg, c(52041,50432,50235))
#' 
#' ## Check Olea capensis again
#' summary(subset(Kenya_veg@species, grepl("Olea capensis", TaxonName),
#' 	   slot="names"), "all")
#' 
#' ## Effect of taxa2samples by counting taxa
#' count_taxa(Kenya_veg, level="genus")
#' 
#' Kenya_veg <- taxa2samples(Kenya_veg, merge_to="genus")
#' count_taxa(Kenya_veg, level="genus")
#' 
#' @rdname merge_taxa
#' 
#' @aliases merge_taxa,vegtable,numeric,missing-method
#' 
#' @exportMethod merge_taxa
#' 
setMethod("merge_taxa", signature(object="vegtable", concepts="numeric",
				level="missing"),
		function(object, concepts, ...) {
			object@species <- merge_taxa(object@species, concepts, ...)
			return(object)
		}
)

#' @rdname merge_taxa
#' 
#' @aliases merge_taxa,vegtable,missing,missing-method
setMethod("merge_taxa", signature(object="vegtable", concepts="missing",
				level="character"),
		function(object, level, ...) {
			object@species <- merge_taxa(object@species, level=level, ...)
			return(object)
		}
)

#' @rdname merge_taxa
#' 
#' @aliases taxa2samples
#' 
#' @exportMethod taxa2samples
#' 
setGeneric("taxa2samples",
		function(object, ...)
			standardGeneric("taxa2samples")
)

#' @rdname merge_taxa
#' 
#' @aliases taxa2samples,vegtable-method
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
