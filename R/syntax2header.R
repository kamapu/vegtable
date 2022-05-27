#' @name syntax2header
#' 
#' @title Add information from slot 'syntax' into slot 'header'
#' 
#' @description 
#' Variables included in different slots of a [taxlist-class] object.
#' 
setGeneric(
    "syntax2header",
    function(object, ...) {
      standardGeneric("syntax2header")
    }
)


setMethod("syntax2header", signature(object = "vegtable"),
    function(object, ...) {
      # TaxonName
      # AcceptedName
      # AuthorName
      # Level
      # ViewID
      # Attributes
    })




