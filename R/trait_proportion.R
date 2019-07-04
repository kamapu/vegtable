# TODO:   Calculate proportions of trait levels in plots, etc.
# 
# Author: Miguel Alvarez
################################################################################

# Set generic function
setGeneric("trait_proportion",
		function(trait, object, ...)
			standardGeneric("trait_proportion")
)

# Method for character
setMethod("trait_proportion", signature(trait="character", object="vegtable"),
		function(trait, object, head_var, trait_level, taxon_level, merge_to,
				include_nas=TRUE, weight, suffix="_prop", in_header=FALSE,
				...) {
			object_in <- object
			object@species <- tax2traits(object@species, get_names=TRUE)
			# Cross-check
			if(!trait %in% colnames(object@species@taxonTraits))
				stop("Value of argument 'trait' is not a taxon trait in the input object.")
			if(!missing(head_var))
				if(!head_var %in% colnames(object@header))
					stop("Value of argument 'head_var' is not a variable at header.")
			if(!missing(trait_level))
				if(any(!trait_level %in%
								paste(object@species@taxonTraits[,trait])))
					stop("Some values in argument 'trait_level' are not included in variable trait.")
			if(!missing(taxon_level))
				if(!taxon_level %in% taxlist::levels(object@species))
					stop("Value of argument 'taxon_level' is not included in the taxonomic list.")
			if(!missing(merge_to))
				if(!merge_to %in% taxlist::levels(object@species))
					stop("Value of in argument 'merge_to' is not included in the taxonomic list.")
			if(!missing(weight))
				if(!weight %in% colnames(object@samples))
					stop("Value of argument 'weight' is not included at slot samples.")
			# Transfer taxonomy to traits
			object@species@taxonTraits[,trait] <-
					paste(object@species@taxonTraits[,trait])
			object@species@taxonTraits[,trait] <-
					replace_x(object@species@taxonTraits[,trait], c(NA, ""),
							c("NA","NA"))
			if(!include_nas)
				object@species@taxonTraits[,trait] <-
						replace_x(object@species@taxonTraits[,trait], c("NA"),
								NA)
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
			if(!missing(weight)) {
				object <- aggregate(as.formula(paste(weight, "~", head_var, "+",
										trait, "+ TaxonConceptID + Level")),
						object@samples, sum)
				object <- crosstable(as.formula(paste(weight, "~", trait, "+",
										head_var)), object, sum,
						na_to_zero=TRUE)
				names(object)[-1] <- paste0(names(object)[-1], suffix)
			} else {
				object@samples$tax_count <- 1
				object <- aggregate(as.formula(paste("tax_count ~", head_var,
										"+", trait,
										"+ TaxonConceptID + Level")),
						object@samples, function(x) 1)
				object <- crosstable(as.formula(paste("tax_count ~", trait, "+",
										head_var)), object, sum,
						na_to_zero=TRUE)
				names(object)[-1] <- paste0(names(object)[-1], suffix)
			}
			# Calculation of proportions
			object <- cbind(object[,1], sweep(object[,-1], 1,
							rowSums(object[,-1]), "/"))
			# Subset to requested levels
			if(!missing(trait_level))
				object <- object[,c(colnames(object)[1], paste0(trait_level,
										suffix))]
			# Finally the output
			names(object)[1] <- head_var
			if(in_header) {
				for(i in colnames(object)[-1])
					object_in@header[,i] <- object[match(object_in$ReleveID,
									object$ReleveID), i]
				return(object_in)
			} else {
				return(object)
			}
		}
)

# Method for formula
setMethod("trait_proportion", signature(trait="formula", object="vegtable"),
		function(trait, object, in_header=FALSE, ...) {
			head_var <- all.vars(update(trait, 0 ~ .))
			trait <-  all.vars(update(trait, . ~ 0))
			OUT <- list()
			for(i in trait) {
				cat(paste0("Processing trait ", i, "...\n"))
				OUT[[i]] <- trait_proportion(i, object, head_var=head_var,
						in_header=FALSE, ...)
			}
			cat("DONE\n")
			if(in_header & head_var != "ReleveID")
				warning("To insert in header 'ReleveID' is required as right term in the formula.")
			if(in_header & head_var == "ReleveID")
				for(i in trait)
					for(j in colnames(OUT[[i]])[-1])
						object@header[,j] <- with(OUT[[i]],
								get(j)[match(object@header$ReleveID, ReleveID)])
			if(!in_header | (in_header & head_var != "ReleveID")) {
				object <- data.frame(Var1=unique(object@header[,head_var]),
						stringsAsFactors=FALSE)
				colnames(object) <- head_var
				for(i in trait)
					for(j in colnames(OUT[[i]])[-1])
						object[,j] <- with(OUT[[i]],
								get(j)[match(object$ReleveID, get(head_var))])
			}
			return(object)
		}
)
