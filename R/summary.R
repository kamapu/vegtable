# TODO:   Summary methods for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# Method for 'vegtable' objects ------------------------------------------------
setMethod("summary", signature(object="vegtable"),
        function(object, units="Kb", ...) {
            # Show original attributes (metadata)
            cat("## Metadata", "\n")
            if(length(object@description) > 0) {
                for(i in names(object@description)) {
                    cat(i, ": ", object@description[i], sep="", "\n")
                }
            }
            cat("object size:", format(object.size(object), units=units),
                    sep=" ", "\n")
            cat("validity:", validObject(object), "\n")
            cat("\n")
            # Content of some slots
            cat("## Content", "\n")
            cat("number of plots:", nrow(object@header), sep=" ", "\n")
            cat("variables in header:", ncol(object@header), sep=" ", "\n")
            cat("number of relations:", length(object@relations), sep=" ", "\n")
            cat("\n")
            # Content of species list
            cat("## Species List", "\n")
            cat("taxon names:", nrow(object@species@taxonNames), sep=" ", "\n")
            cat("taxon concepts:", nrow(object@species@taxonRelations), sep=" ",
                    "\n")
            cat("validity:", validObject(object@species), sep=" ", "\n")
            cat("\n")
        }
)

# Method for 'coverconvert' objects --------------------------------------------
setMethod("summary", signature(object="coverconvert"),
		function(object, ...) {
			cat("## Number of cover scales:", length(object@value), "\n")
			cat("\n")
			for(i in names(object@value)) {
				Levels <- paste(object@value[[i]])
				Range_1 <- paste(object@conversion[[i]])[
						-length(object@conversion[[i]])]
				Range_2 <- paste(object@conversion[[i]])[-1]
				for(j in 1:length(Range_2))
					if(duplicated(Range_2)[j]) Range_1[j] <- Range_1[j - 1]
				cat(paste0("* scale '", i, "':"), "\n")
				print(data.frame(Levels=Levels, Range=paste(Range_1, "-",
										Range_2), stringsAsFactors=FALSE))
				cat("\n")
			}
		}
)

# Method for 'shaker' objects --------------------------------------------------
setMethod("summary", signature(object="shaker"),
		function(object, companion, authority=FALSE, ...) {
			if(missing(companion)) {
				cat("Number of pseudo-species:", length(object@pseudos), "\n")
				cat("Number of species groups:", length(object@groups), "\n")
				cat("Number of formulas:", length(object@formulas), "\n")
			}
			if(!missing(companion)) {
				if(class(companion) == "vegtable")
					companion <- companion@species
				companion <- accepted_name(companion)
				if(authority) {
					companion$AuthorName[is.na(companion$AuthorName)] <- ""
					companion$TaxonName <- with(companion, paste(TaxonName,
									AuthorName))
				}
				if(length(object@pseudos) > 0) {
					cat("## Pseudo-species:", "\n")
					for(i in 1:length(object@pseudos)) {
						cat("*", paste0("'",
										companion[match(object@pseudos[[i]][1],
														companion$TaxonConceptID),
												"TaxonName"], "'"),
								"contains:", "\n")
						for(j in 2:length(object@pseudos[[i]])) {
							cat("    ", companion[match(object@pseudos[[i]][j],
													companion$TaxonConceptID),
											"TaxonName"], "\n")
						}
					}
					cat("\n")
				}
				if(length(object@groups) > 0) {
					cat("## Species groups:", "\n")
					for(i in 1:length(object@groups)) {
						cat("*", paste0("'", names(object@groups)[i],
										"' group:"), "\n")
						for(j in 1:length(object@groups[[i]])) {
							cat("    ", companion[match(object@groups[[i]][j],
													companion$TaxonConceptID),
											"TaxonName"], "\n")
						}
					}
					cat("\n")
				}
				if(length(object@formulas) > 0) {
					cat("## Formulas:", "\n")
					EQ <- rewrite_formulas(object, companion)
					for(i in 1:length(object@formulas)) {
						cat("*", paste0(names(object@formulas)[i], ":"),
								EQ[[i]], "\n")
					}
					cat("\n")
				}
			}
		}
)
