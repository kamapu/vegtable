#' @name names
#' 
#' @title Retrieve names of vegtable and coverconvert objects
#' 
#' @description 
#' Quick access to column names in slot header and names of conversion codes.
#' 
#' These methods provide a quick display of the contents in
#' [coverconvert-class] and [vegtable-class] objects.
#' 
#' @param x An object of class [coverconvert-class] or [vegtable-class].
#' @param value A character vector used for replacement methods.
#' 
#' @return Either a vector or a list (in the case of `dimnames()`) with
#' the names of variables.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}.
#' 
#' @examples
#' names(Kenya_veg@@coverconvert)
#' names(Kenya_veg)
#' dimnames(Kenya_veg)
#' 
#' @rdname names
#' @aliases names,vegtable-method
#' 
#' @exportMethod names
#' 
setMethod("names", signature(x="vegtable"),
        function(x) colnames(x@header)
)

#' @rdname names
#' @aliases names<- names<-,vegtable-method
#' 
#' @exportMethod names<-
#' 
setReplaceMethod("names", signature(x="vegtable"),
        function(x, value) {
            colnames(x@header) <- value
            return(x)
        }
)

#' @rdname names
#' @aliases dimnames dimnames,vegtable-method
#' 
#' @exportMethod dimnames
#' 
setMethod("dimnames", signature(x="vegtable"),
        function(x) return(list(x@header$ReleveID, colnames(x@header)))
)

#' @rdname names
#' @aliases names,coverconvert-method
#' 
setMethod("names", signature(x="coverconvert"),
        function(x) names(x@value)
)

#' @rdname names
#' @aliases names<-,coverconvert-method
#' 
setReplaceMethod("names", signature(x="coverconvert"),
        function(x, value) {
            names(x@value) <- value
            names(x@conversion) <- value
            return(x)
        }
)
