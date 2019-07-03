# TODO:   Calculate proportions of trait levels in plots, etc.
# 
# Author: Miguel Alvarez
################################################################################

# Set generic function
setGeneric("trait_stats",
		function(trait, object, ...)
			standardGeneric("trait_stats")
)

# Method for character
setMethod("trait_stats", signature(trait="character", object="vegtable"),
		function(trait, object, FUN, head_var, taxon_level, merge_to, weight,
				suffix="_stats", in_header=FALSE, ...) {
			object_in <- object
			object@species <- tax2traits(object@species, get_names=TRUE)
			
			# Cross-check
			if(!trait %in% colnames(object@species@taxonTraits))
				stop("Value of argument 'trait' is not a taxon trait in the input object.")
			if(!missing(head_var))
				if(!head_var %in% colnames(object@header))
					stop("Value of argument 'head_var' is not a variable at header.")
			if(!missing(taxon_level))
				if(!taxon_level %in% taxlist::levels(object@species))
					stop("Value of argument 'taxon_level' is not included in the taxonomic list.")
			if(!missing(merge_to))
				if(!merge_to %in% taxlist::levels(object@species))
					stop("Value of in argument 'merge_to' is not included in the taxonomic list.")
			if(!missing(weight))
				if(!weight %in% colnames(object@samples))
					stop("Value of argument 'weight' is not included at slot samples.")
			# Transfer traits to samples
			if(missing(merge_to))
				object <- taxa2samples(object, add_traits=trait) else
				object <- taxa2samples(object, merge_to, trait)
			
			# Transfer head variable to samples
			if(!missing(head_var))
				object@samples[,head_var] <- with(object@header,
						replace_x(object@samples$ReleveID, ReleveID,
								get(head_var))) else head_var <- "ReleveID"
			object@samples <- object@samples[!is.na(object@samples[,trait]),]
			# Aggregate to taxon
			object@samples$Level <- with(object@species@taxonRelations,
					paste(Level)[match(object@samples$TaxonConceptID,
									TaxonConceptID)])
			if(!missing(taxon_level))
				object@samples <- object@samples[object@samples$Level ==
								taxon_level,]
			# In case of weighted statistics
			if(!missing(weight)) {
				idx <- unique(object@samples[,head_var])
				x <- with(object@samples, split(get(trait), get(head_var)))
				w <- with(object@samples, split(get(weight), get(head_var)))
				object <- data.frame(Var1=idx, Var2=mapply(FUN, x=x, w=w, ...),
						stringsAsFactors=FALSE)
				colnames(object) <- c(head_var, trait)
			} else {
				object <- aggregate(as.formula(paste(trait, "~", head_var)),
						object@samples, FUN, ...)
			}
			# Finally the output
			if(in_header) {
				object_in@header[,paste0(trait, suffix)] <-
						object[match(object_in@header[,head_var],
										object[,head_var]),trait]
				return(object_in)
			} else {
				colnames(object)[colnames(object) == trait] <- paste0(trait,
						suffix)
				return(object)
			}
		}
)

# Method for formula
setMethod("trait_stats", signature(trait="formula", object="vegtable"),
		function(trait, object, weight, suffix="_stats", in_header=FALSE, ...) {
			head_var <- all.vars(update(trait, 0 ~ .))
			trait <-  all.vars(update(trait, . ~ 0))
			OUT <- list()
			for(i in trait) {
				OUT[[i]] <- trait_stats(i, object, head_var=head_var,
						weight=weight, suffix=suffix, in_header=FALSE, ...)
			}
			if(in_header & head_var != "ReleveID")
				warning("To insert in header 'ReleveID' is required as right term in the formula.")
			if(in_header & head_var == "ReleveID")
				for(i in trait)
					object@header[,paste0(i, suffix)] <- with(OUT[[i]],
							get(paste0(i, suffix))[match(object@header$ReleveID,
											ReleveID)])
			if(!in_header | (in_header & head_var != "ReleveID")) {
				object <- data.frame(Var1=unique(object@header[,head_var]),
						stringsAsFactors=FALSE)
				colnames(object) <- head_var
				for(i in trait)
					object[,paste0(i, suffix)] <- with(OUT[[i]],
							get(paste0(i, suffix))[match(object[,head_var],
											get(head_var))])
			}
			return(object)
		}
)
