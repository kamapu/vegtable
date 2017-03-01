# TODO:   Retrieve or replace slot headerer
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("header",
        function(x, ...)
            standardGeneric("header")
)

# Method for vegtable objects
setMethod("header", signature(x="vegtable"),
        function(x, ...) {
            return(x@header)
        }
)

# Generic for replacement method
setGeneric("header<-", function(x, value)
            standardGeneric("header<-"))

# Replacement method
setReplaceMethod("header", signature(x="vegtable", value="data.frame"),
        function(x, value) {
            rownames(value) <- paste(value$ReleveID)
            if(!all(x@samples$RELEVE_NR %in% value$ReleveID))
                stop("Some plots are missing in 'value'")
            x@header <- value[value$RELEVE_NR %in% x@samples$ReleveID,]
            # Popups?
            return(x)
        }
)
