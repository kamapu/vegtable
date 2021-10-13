#' @name summary
#' 
#' @title Summary method for vegtable objects
#' 
#' @description 
#' Display summaries for [vegtable-class] objects.
#' 
#' Those methods are implemented for objects of the classes [vegtable-class],
#' [coverconvert-class] and [shaker-class].
#' 
#' The method for class `vegtable` retrieves the metadata, the size of
#' the object, its validity and additional statistics on the content of input
#' object.
#' 
#' For objects of class [shaker-class], the function `summary()` will either
#' retrieve general statistics when `companion` is missing, or a more detailed
#' display when accompained by a [taxlist-class] or [vegtable-class] object.
#' 
#' @param object,x Object to be summarized.
#' @param units Units used for object size (passed to [format()]).
#' @param companion Companion object (either a [taxlist-class] or a
#'     [vegtable-class] object.
#' @param authority Logical value indicating whether authors should be
#'     displayed or not.
#' @param ... further arguments to be passed to or from other methods.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples
#' ## Summary for 'vegtable' objects
#' summary(Wetlands_veg)
#' 
#' @rdname summary
#' @aliases summary,vegtable-method
#' 
#' @exportMethod summary
#' 
setMethod("summary", signature(object = "vegtable"),
        function(object, units = "Kb", ...) {
            # Show original attributes (metadata)
            cat("## Metadata", "\n")
            if(length(object@description) > 0) {
                for(i in names(object@description)) {
                    cat("   ", i, ": ", object@description[i], sep = "", "\n")
                }
            }
            cat("   object size:", format(object.size(object), units = units),
                    sep = " ", "\n")
            cat("   validity:", validObject(object), "\n")
            cat("\n")
            # Content of some slots
            cat("## Content", "\n")
            cat("   number of plots:", nrow(object@header), sep = " ", "\n")
			cat("   plots with records:",
					length(unique(object@samples$ReleveID)), sep = " ", "\n")
			cat("   variables in header:", ncol(object@header), sep = " ", "\n")
            cat("   number of relations:", length(object@relations), sep = " ",
					"\n")
            cat("\n")
            # Content of species list
            cat("## Taxonomic List", "\n")
            cat("   taxon names:", nrow(object@species@taxonNames), sep = " ",
					"\n")
            cat("   taxon concepts:", nrow(object@species@taxonRelations),
					sep = " ",
                    "\n")
            cat("   validity:", validObject(object@species), sep = " ", "\n")
            cat("\n")
        }
)

#' @rdname summary
#' @aliases summary,coverconvert-method
#' 
#' @examples
#' ## Summary for 'coverconvert' objects
#' summary(braun_blanquet)
#' 
setMethod("summary", signature(object = "coverconvert"),
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
				print(data.frame(Levels = Levels, Range = paste(Range_1, "-",
										Range_2), stringsAsFactors = FALSE))
				cat("\n")
			}
		}
)

#' Re-writing formulas for print output
#' 
#' Mediating between syntax and print format.
#' 
#' @param shaker A [shaker-class] object.
#' @param companion A companion data set which is either missing or a
#'     [vegtable-class] object.
#' 
#' @return A formated output text.
#' 
#' @keywords internal
rewrite_formulas <- function(shaker, companion) {
	EQ <- list()
	for(i in names(shaker@formulas)) EQ[[i]] <- {
			x <- shaker@formulas[[i]]
			if(grepl("\'", x)) SYM <- "\'"
			if(grepl('\"', x)) SYM <- '\"'
			x <- gsub("groups[[", "groups:", x, fixed = TRUE)
			x <- gsub("dominants[[", "species:", x, fixed = TRUE)
			x <- gsub("]]", "", x, fixed = TRUE)
			Spp <- as.numeric(unlist(regmatches(x,
									gregexpr("[[:digit:]]+\\.*[[:digit:]]*",
											x))))
			if(length(Spp) > 0){
				for(j in Spp) {
					subformula <- shaker@dominants[j,]
					subformula$TaxonConceptID <- companion[
							match(subformula$TaxonConceptID,
									companion$TaxonConceptID),"TaxonName"]
					subformula <- paste(subformula[1,], collapse = " ")
					
					x <- sub(paste0("species:", j), paste0("species:", SYM,
									subformula, SYM), x)
				}
			}
			x
		}
	return(EQ)
}

#' @rdname summary
#' @aliases summary,shaker-method
#' 
#' @examples
#' ## Summary for 'shaker' objects (alone and with companion)
#' summary(Wetlands, Wetlands_veg)
#' 
setMethod("summary", signature(object = "shaker"),
		function(object, companion, authority = FALSE, ...) {
			if(missing(companion)) {
				cat("Number of pseudo-species:", length(object@pseudos), "\n")
				cat("Number of species groups:", length(object@groups), "\n")
				cat("Number of formulas:", length(object@formulas), "\n")
			} else {
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

################################################################################

#' @rdname summary
#' 
#' @aliases show,vegtable-method
#' 
#' @exportMethod show
setMethod("show", signature(object = "vegtable"),
    function(object) {
      summary(object)
    }
)

#' @rdname summary
#' 
#' @aliases print,vegtable-method
setMethod("print", signature(x = "vegtable"),
    function(x, ...) {
      summary(x, ...)
    }
)

################################################################################

#' @rdname summary
#' 
#' @aliases show,coverconvert-method
#' 
#' @exportMethod show
setMethod("show", signature(object = "coverconvert"),
    function(object) {
      summary(object)
    }
)

#' @rdname summary
#' 
#' @aliases print,coverconvert-method
setMethod("print", signature(x = "coverconvert"),
    function(x, ...) {
      summary(x, ...)
    }
)

################################################################################

#' @rdname summary
#' 
#' @aliases show,shaker-method
#' 
#' @exportMethod show
setMethod("show", signature(object = "shaker"),
    function(object) {
      summary(object)
    }
)

#' @rdname summary
#' 
#' @aliases print,shaker-method
setMethod("print", signature(x = "shaker"),
    function(x, ...) {
      summary(x, ...)
    }
)
