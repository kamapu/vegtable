#' @name count_taxa
#' @aliases count_taxa,vegtable,missing-method
#' 
#' @title Count taxa included in vegtable objects
#' 
#' @description 
#' Counting number of taxa within [taxlist-class] objects or character vectors
#' containing taxon names.
#' 
#' This function provides a quick calculation of taxa in [vegtable-class]
#' objects, considering only records in slot samples.
#' Such records can be also merged from lower ranks.
#' 
#' For the formula method, units without any requested taxa will not appear in
#' the output data frame. If no taxa at all is occurring at the requested level
#' in any unit, an error message will be retrieved.
#' 
#' @param object An object of class [vegtable-class] or a formula.
#' @param value A formula passed to parameter 'object' by the replace method.
#' @param data An object of class [vegtable-class].
#' @param level Character value indicating the taxonomic rank of counted taxa.
#' @param include_lower Logical value, whether lower taxonomic ranks should be
#'     included at the requested level.
#' @param suffix Character value used as suffix on the calculated variable.
#' @param in_header Logical value, whether the result should be included in the
#'     slot header of the input [vegtable-class] object or not.
#'     A warning message is provided if the calculation is not done for every
#'     plot observation.
#' @param ... further arguments passed among methods.
#' 
#' @return
#' An data frame with the number of taxa from requested level at requested
#' units for the formula method, or just an integer value.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples
#' ## Different alternatives
#' count_taxa(Kenya_veg)
#' head(count_taxa(~ ReleveID, Kenya_veg))
#' head(count_taxa(species ~ ReleveID, Kenya_veg))
#' head(count_taxa(species ~ ReleveID, Kenya_veg, TRUE))
#' head(count_taxa(family ~ ReleveID, Kenya_veg, TRUE))
#' 
#' @rdname count_taxa
#' 
#' @exportMethod count_taxa
#' 
setMethod("count_taxa", signature(object="vegtable", data="missing"),
		function(object, level, include_lower=FALSE, ...) {
			concepts <- with(object@species@taxonNames,
					TaxonConceptID[match(object@samples$TaxonUsageID,
									TaxonUsageID)])
			if(!missing(level))
				if(!level %in% levels(object@species))
					stop(paste("Value of argument 'level' is not a level",
									"in 'object'."))
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

#' @rdname count_taxa
#' 
#' @aliases count_taxa,formula,vegtable-method
setMethod("count_taxa", signature(object = "formula", data = "vegtable"),
		function(object, data, include_lower = FALSE, suffix = "_count",
				in_header = FALSE, ...) {
			data_in <- data
			nr_response <- attr(terms(object), "response")
			name_response <- as.character(object)[2]
			if(nr_response > 1)
				stop("More than one response in formula are not allowed.")
			if(nr_response == 1 & include_lower)
				data <- taxa2samples(data, name_response) else
				data <- taxa2samples(data)
			if(nr_response == 1) {
				concepts <- with(data@species@taxonNames,
						TaxonConceptID[match(data@samples$TaxonUsageID,
										TaxonUsageID)])
				concept_levels <- with(data@species@taxonRelations,
						as.integer(Level)[match(concepts, TaxonConceptID)])
				data@samples$TaxonUsageID[!concept_levels ==
								which(levels(data@species) == name_response)] <-
						NA
			}
			object <- as.formula(paste("TaxonUsageID ~",
							paste(attr(terms(object), "term.labels"),
									collapse=" + ")))
			if(all(is.na(data@samples$TaxonUsageID)))
				stop("No records for requested taxon rank.")
			data <- aggregate(object, data, function(x) length(unique(x)), ...)
			if(name_response == "ReleveID") name_response <- "taxa"
			colnames(data)[colnames(data) == "TaxonUsageID"] <-
					paste0(name_response, suffix)
			if(colnames(data)[1] != "ReleveID" & in_header)
				warning("'ReleveID' is not included as factor in formula")
			if(colnames(data)[1] == "ReleveID" & in_header) {
				data_in@header[,colnames(data)[2]] <- with(data_in@header,
						data[match(ReleveID, data$ReleveID),2])
				return(data_in)
			} else return(data)
		}
)

#' @rdname count_taxa
#' 
#' @aliases count_taxa<-
#' 
#' @exportMethod count_taxa<-
#' 
setGeneric("count_taxa<-", function(data, ..., value)
			standardGeneric("count_taxa<-"))

#' @rdname count_taxa
#' 
#' @aliases count_taxa<-,vegtable,formula-method
#' 
setReplaceMethod("count_taxa", signature(data = "vegtable", value = "formula"),
		function(data, ..., value) {
			return(count_taxa(object = value, data = data, in_header = TRUE,
							...))
		})
