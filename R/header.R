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
            if(!"ReleveID" %in% colnames(value))
                stop("Column 'ReleveID' is mandatory in 'value'")
            for(i in colnames(value)) {
                x@header[,i] <- value[match(x@header$ReleveID, value$ReleveID),
                        i]
            }
            return(x)
        }
)
