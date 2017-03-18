# TODO:   Retrieve the names of 'vegtable' and 'coverconvert' classes
# 
# Author: Miguel Alvarez
################################################################################

# Methods for 'vegtable' -------------------------------------------------------

# Access to names of class 'vegtable' (header variables)
setMethod("names", signature(x="vegtable"),
        function(x) colnames(x@header)
)

# Replacement of names in header
setReplaceMethod("names", signature(x="vegtable"),
        function(x, value) {
            colnames(x@header) <- value
            return(x)
        }
)

# Access to both, header variables and releve IDs
setMethod("dimnames", signature(x="vegtable"),
        function(x) return(list(x@header$ReleveID, colnames(x@header)))
)

# Methods for 'coverconvert' ---------------------------------------------------

# Access to names of class 'coverconvert'
setMethod("names", signature(x="coverconvert"),
        function(x) names(x@value)
)

# Replacement for names of scales
setReplaceMethod("names", signature(x="coverconvert"),
        function(x, value) {
            names(x@value) <- value
            names(x@conversion) <- value
            return(x)
        }
)
