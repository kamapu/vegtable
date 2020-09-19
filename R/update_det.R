#' @name update_det
#' 
#' @title Update by determined specimens
#' 
#' @description 
#' Reference specimens can be integrated in slot **layers** within a
#' [vegtable-class] object.
#' Updated entries in the specimens can be updated in slot **samples** by using
#' this function.
#' Alternatively expert opinions can be inserted and applied in case of
#' disagreement with the original records.
#' 
#' @param x A [vegtable-class] object to be updated.
#' @param specimens A character vector indicating the names of tables included
#'     in slot **layers** with updates to be applied.
#'     Note that they will be applied in the same order of the vector in the
#'     case of multiple updates.
#' @param ... Further arguments (not yet in use).
#' 
#' @rdname update_det
#' 
#' @exportMethod update_det
#' 
setGeneric("update_det",
		function(x, specimens, ...)
			standardGeneric("update_det")
)

#' @rdname update_det
#' 
#' @aliases update_det,vegtable-method
#' 
setMethod("update_det", signature(x="vegtable", specimens="character"),
		function(x, specimens, ...) {
			if(any(!specimens %in% names(x@layers)))
				stop(paste("Some values in 'specimens' are not included",
								"as layer in 'x'."))
			for(i in specimens) {
				if(!"TaxonUsageID" %in% colnames(x@layers[[i]]))
					stop(paste0("At \"", i, "\", column 'TaxonUsageID' is ",
									"mandatory in list of specimens."))
				if(any(!x@layers[[i]]$TaxonUsageID %in%
								x@species@taxonRelations$TaxonUsageID))
					stop(paste0("At \"", i, "\", some taxon usage names ",
									"are not included in slot 'species'."))
				x@samples$TaxonUsageID <- replace_idx(x@samples$TaxonUsageID,
						x@samples[ ,i], x@layers[[i]][ ,i],
						x@layers[[i]]$TaxonUsageID)
			}
			
			# In the case that taxa2samples was applied before
			if("TaxonConceptID" %in% colnames(x@samples))
				warning("You may like to repeat 'taxa2samples()' on 'x'.")
			return(x)
		})
