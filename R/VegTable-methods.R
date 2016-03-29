# TODO:   Methods for "VegTable" objects
#		  - print
#         - subset
# 
# Author: Miguel Alvarez
################################################################################

# print ------------------------------------------------------------------------
setMethod(f="print", signature="VegTable",
		function(x, ...) {
			# Print original attributes (metadata)
			if(length(x@description) > 0) for(i in names(x@description)) {
					cat(i, ": ", x@description[[i]], sep="", "\n")
				}
			cat("\n")
			# Print dimensions of database
			cat(dim(x@head)[1], " observations of ", dim(x@species)[1],
					" species.", sep="", "\n")
			cat(dim(x@head)[2], " variables with records.", sep="", "\n")
			cat("\n")
			# Print logged actions
			if(length(x@log$import) > 0) cat("[", x@log$import["time"],
						"]: data imported from '", x@log$import["database"],
						"'", sep="", "\n")
			cat("\n")
			if(length(x@log$deleted.plots) > 0) {
				cat("Plots deleted from original", sep="", "\n")
				for(i in 1:length(x@log$deleted.plots)) {
					if(length(x@log$deleted.plots[[i]]) > 50) {
						x@log$deleted.plots[[i]] <-
								c(x@log$deleted.plots[[i]][1:50],
										"... [TRUNCATED]")
					}
					cat(paste0("[", attr(x@log$deleted.plots, "time")[i], "]:",
									collapse=""), x@log$deleted.plots[[i]],
							sep=" ", fill=TRUE)
				}
			}
			cat("\n")
			if(length(x@log$deleted.species) > 0) {
				cat("Species deleted from original", sep="", "\n")
				for(i in 1:length(x@log$deleted.species)) {
					if(length(x@log$deleted.species[[i]]) > 50) {
						x@log$deleted.species[[i]] <-
								c(x@log$deleted.species[[i]][1:50],
										"... [TRUNCATED]")
					}
					cat(paste0("[", attr(x@log$deleted.species,
											"time")[i], "]:", collapse=""),
							x@log$deleted.species[[i]], sep=" ", fill=TRUE)
				}
			}
			cat("\n")
			if(length(x@log$replaced.species) > 0) {
				cat("Species replaced by 'poolssp'", sep="", "\n")
				for(i in 1:length(x@log$replaced.species)) {
					if(length(x@log$replaced.species[[i]]) > 50) {
						x@log$replaced.species[[i]] <-
								c(x@log$replaced.species[[i]][1:50],
										"... [TRUNCATED]")
					}
					cat(paste0("[", attr(x@log$replaced.species,
											"time")[i], "]:", collapse=""),
							x@log$replaced.species[[i]], sep=" / ", fill=TRUE)
				}
			}
			cat("\n")
		})

# subset -----------------------------------------------------------------------

# 1: by head or species
# (return logical vector for rownames of the inserted data.frame)
# x=vegtable@head or x=vegtable@species
subset.table <- function(x, subset) {
	r <- eval(subset, x, parent.frame())
	if (!is.logical(r)) stop("'subset' must be logical")
	r <- r & !is.na(r)
	return(r)
}

# 2: by popups (return logical vector for head rownames)
# Note that this function requires x=vegtable
subset.popup <- function(x, subset) {
	vars <- list()
	for(i in names(subset)) {
		if(i %in% names(x@popups)) {
			r <- eval(subset[[i]], x@popups[[i]])
			if (!is.logical(r)) stop("'subset' must be logical")
			r <- r & !is.na(r)
			vars[[colnames(x@popups[[i]])[1]]] <- x@popups[[i]][r,1,
					drop=TRUE]
		}
	}
	out <- list()
	for(i in names(vars)) {
		out[[i]] <- x@head[,i] %in% vars[[i]]
	}
	r <- apply(as.data.frame(out), 1, all)
	return(r)
}

# 3: The final function as method
setMethod(f="subset", signature="VegTable",
		function(x, plots, species, popups, ...) {
			# Record of plots and species in input
			plots.in <- rownames(x@head);species.in <- rownames(x@species)
			# Subset as logical output
			if(!missing(species)) sub.species <- subset.table(x@species,
						substitute(species)) else {
				sub.species <- rep_len(TRUE, nrow(x@species))
			}
			if(!missing(plots)) sub.plots <- subset.table(x@head,
						substitute(plots)) else {
				sub.plots <- rep_len(TRUE, nrow(x@head))
			}
			if(!missing(popups)) sub.popups <- subset.popup(x, popups) else {
				sub.popups <- rep_len(TRUE, nrow(x@head))
			}
			# Starting subset in the sample
			x@samples <- subset(x@samples,
					RELEVE_NR %in% x@head$RELEVE_NR[sub.plots & sub.popups])
			x@samples <- subset(x@samples,
					TaxonConceptID %in% x@species$TaxonConceptID[sub.species])
			# Back to head and species
			x@head <- subset(x@head, RELEVE_NR %in% x@samples$RELEVE_NR)
			x@species <- subset(x@species,
					TaxonConceptID %in% x@samples$TaxonConceptID)
			# Compare input with output objects
			plots.out <- plots.in[!plots.in %in% rownames(x@head)]
			species.out <- species.in[!species.in %in% rownames(x@species)]
			if(length(plots.out) > 0) {
				x@log$deleted.plots[[length(x@log$deleted.plots) + 1]] <-
						plots.out
				attr(x@log$deleted.plots, "time")[length(
								x@log$deleted.plots)] <- paste(Sys.time())
			}
			if(length(species.out) > 0) {
				x@log$deleted.species[[length(x@log$deleted.species) + 1]] <-
						species.out
				attr(x@log$deleted.species, "time")[length(
								x@log$deleted.species)] <- paste(Sys.time())
			}
			return(x)
		})
