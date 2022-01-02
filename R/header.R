#' @name header
#'
#' @title Retrieve or replace slot header in vegtable objects
#'
#' @description
#' Retrieve or replace the content of slot `header` in [vegtable-class] objects.
#'
#' @param x Object of class [vegtable-class].
#' @param value Data frame to be set as slot `header`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' head(header(Kenya_veg))
#' @rdname header
#'
#' @exportMethod header
#'
setGeneric(
  "header",
  function(x, ...) {
    standardGeneric("header")
  }
)

#' @rdname header
#'
#' @aliases header,vegtable-method
setMethod(
  "header", signature(x = "vegtable"),
  function(x, ...) {
    return(x@header)
  }
)

#' @rdname header
#'
#' @aliases header<-
#'
#' @exportMethod header<-
#'
setGeneric("header<-", function(x, value) {
  standardGeneric("header<-")
})

#' @rdname header
#'
#' @aliases header<-,vegtable,data.frame-method
setReplaceMethod(
  "header", signature(x = "vegtable", value = "data.frame"),
  function(x, value) {
    if (!"ReleveID" %in% colnames(value)) {
      stop("Column 'ReleveID' is mandatory in 'value'")
    }
    for (i in colnames(value)) {
      x@header[, i] <- value[
        match(x@header$ReleveID, value$ReleveID),
        i
      ]
    }
    return(x)
  }
)
