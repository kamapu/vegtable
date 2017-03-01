# TODO:   Summary methods for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

setMethod(f="summary", signature(object="vegtable"),
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
        })
