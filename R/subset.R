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
#' @param subset Logical vector or operation for subset.
#' @param slot Slot to be applied for subset.
#' @param keep_children Argument passed to [taxlist::get_children()].
#' @param keep_parents Argument passed to [taxlist::get_parents()].
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
				keep_parents=FALSE, ...) {
			# In the case of a subset by species
			if(any(grepl(slot[1], slotNames(x@species), ignore.case=TRUE))) {
				# here it is a copy of taxlist::subset
				slot <- grep(slot[1], slotNames(x@species), ignore.case=TRUE)
				slot <- slotNames(x@species)[slot]



				subset <- substitute(subset)
				subset <- eval(subset, slot(x@species, slot), parent.frame())
				if(slot %in% c("taxonNames","taxonRelations","taxonTraits"))
					subset <- unique(slot(x@species, slot)[subset,"TaxonConceptID"])
				else if(slot == "taxonViews") {
					subset <- unique(slot(x@species, slot)[subset,"ViewID"])
					subset <- x@species@taxonRelations[
							x@species@taxonRelations$ViewID %in% subset,
							"TaxonConceptID"]
				}
				z <- x@species
				z@taxonRelations <- x@species@taxonRelations[
						x@species@taxonRelations$TaxonConceptID %in% subset,]
				z <- clean(z)
				if(keep_children)
					z <- get_children(x@species, z)
				if(keep_parents)
					z <- get_parents(x@species, z)
				x@species <- z
			} else {
				## slot <- grep(slot[1], slotNames(x), ignore.case=TRUE)
				slot <- grep(slot[1], c("samples","header"), ignore.case=TRUE)
				if(length(slot) == 0)
					stop("Invalid value for argument 'slot'")
				## slot <- slotNames(x)[slot]
				slot <- c("samples","header")[slot]
				subset <- substitute(subset)
				subset <- eval(subset, slot(x, slot), parent.frame())
				if(slot == "samples") x@samples <- x@samples[subset,]
				if(slot == "header") x@header <- x@header[subset,]
			}
			return(clean(x))
        }
)
