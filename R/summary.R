# TODO:   Summary methods for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

setMethod(f="summary", signature(object="vegtable"),
        function(object, ...) {
            # Show original attributes (metadata)
            if(length(object@description) > 0) {
                for(i in names(object@description)) {
                    cat(i, ": ", object@description[i], sep="", "\n")
                }
            }
            cat("\n")
            # Show dimensions of database
            cat(dim(object@head)[1], " observations (plots).", sep="", "\n")
            cat(dim(object@head)[2], " variables with records.", sep="", "\n")
            cat("\n")
            # Show summary of species list
            cat("Summary of species list:", sep="", "\n")
            if(class(object@species) == "taxlist") summary(object@species)
            cat("validation for class 'vegtable':", validObject(object), "\n")
            cat("\n")
        })
