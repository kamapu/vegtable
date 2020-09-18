#' @name subset
#' @aliases subset,vegtable-method
#' 
#' @title Subset functions for vegtable objects
#' 
#' @description 
#' Produce subsets of [vegtable-class] objects.
#' 
#' This function generate subsets of [vegtable-class] objects through logical
#' operations. Such operations can be applied either to the plots, or the
#' relations, which are the main slots in that class.
#' 
#' This method can be referred to the slot `species` the same way as
#' [taxlist::subset()], then the rest of the data will include only
#' references to the subset of species list.
#' 
#' @param x A [vegtable-class] object for subset.
#' @param subset Logical expression for subset.
#' @param slot Character value indicating the slot used as reference for subset.
#'     At the moment only the values "taxonNames", "taxonRelations",
#'     "taxonTraits", "header", "samples", and "relations" are accepted.
#'     The three first values will be applied to the respective slots in the
#'     contained [taxlist-class] object (slot **species**).
#' @param keep_children Argument passed to [taxlist::get_children()].
#' @param keep_parents Argument passed to [taxlist::get_parents()].
#' @param relation Character value indicating the relation (slot **relations**)
#'     to be used as reference for subset.
#' @param ... Further arguments passed from or to other methods.
#' 
#' @return A S4 object of class [vegtable-class].
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples
#' summary(dune_veg)
#' 
#' ## Select plots used as pastures
#' Pastures <- subset(dune_veg, Use == "Pasture", slot="header")
#' summary(Pastures)
#' 
#' @exportMethod subset
#' 
setMethod("subset", signature(x="vegtable"),
        function(x, subset, slot="header", keep_children=FALSE,
				keep_parents=FALSE, relation, ...) {
			p_slots <- c("taxonNames", "taxonRelations", "taxonTraits",
					"header", "samples", "relations")
			if(!slot %in% p_slots)
				stop(paste0("Only following values are allowed for argument ",
								"'slot':\n\"", paste0(p_slots,
										collapse="\" \""), "\"."))
			subset <- substitute(subset)
			# For subsets by taxonomic list
			if(slot %in% p_slots[1:3]) {
				# Duplicated taxlist object
				z <- x@species
				# in taxonNames
				if(slot == p_slots[1]) {
					subset <- eval(subset, z@taxonNames, parent.frame())
					z@taxonNames <- z@taxonNames[subset, ]
				}
				# in taxonRelations
				if(slot == p_slots[2]) {
					subset <- eval(subset, z@taxonRelations, parent.frame())
					z@taxonRelations <- z@taxonRelations[subset, ]
				}
				# in taxonTraits
				if(slot == p_slots[3]) {
					subset <- eval(subset, z@taxonTraits, parent.frame())
					z@taxonTraits <- z@taxonTraits[subset, ]
					z@taxonRelations <- z@taxonRelations[
							z@taxonRelations$TaxonConceptID %in%
									z@taxonTraits$TaxonConceptID, ]
				}
				# clean invalid taxlist object
				z <- clean(z)
				# recover relatives
				if(keep_children)
					z <- get_children(x@species, z)
				if(keep_parents)
					z <- get_parents(x@species, z)
				# back to x
				x@species <- z
			}
			# in header
			if(slot == p_slots[4]) {
				subset <- eval(subset, x@header, parent.frame())
				x@header <- x@header[subset, ]
			}
			# in samples
			if(slot == p_slots[5]) {
				subset <- eval(subset, x@samples, parent.frame())
				x@samples <- x@samples[subset, ]
			}
			# in relations
			if(slot == p_slots[6] & !missing(relation)) {
				if(!relation %in% names(x@relations))
					stop(paste0("\"", relation, "\" is not a relation in x."))
				subset <- eval(subset, x@relations[[relation]], parent.frame())
				x@relations[[relation]] <- x@relations[[relation]][subset, ]
				x@header <- x@header[paste(x@header[ ,relation]) %in%
								paste(x@relations[[relation]][ ,relation]), ]
			}
			# return clean object
			return(clean(x))
        }
)
