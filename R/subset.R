# TODO:   subset functions for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# subset method for vegtable objects
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
