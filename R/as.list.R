#' @name as.list
#' @aliases as.list,vegtable-method
#'
#' @title Coerce an S4 object to a list
#'
#' @description
#' Coercion used to explore content in S4 objects.
#'
#' S4 objects will be coerced to lists, where each slot in the input object
#' becomes a member of the output list. This way allows to explore content and
#' solve problems when validity checks fail.
#'
#' @param x an object of class [coverconvert-class] or [vegtable-class]
#' @param ... further arguments passed from or to other methods.
#' @return An object of class `list`.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## Head of slot 'taxonNames'
#' class(Easplist)
#' head(Easplist@taxonNames)
#'
#' ## The same after coercing to list
#' Easplist <- as.list(Easplist)
#' class(Easplist)
#' head(Easplist$taxonNames)
#' @rdname as.list
#'
#' @exportMethod as.list
#'
setMethod(
  "as.list", signature(x = "vegtable"),
  function(x, ...) {
    S4_to_list(x)
  }
)

#' @rdname as.list
#'
#' @aliases as.list,coverconvert-method
setMethod(
  "as.list", signature(x = "coverconvert"),
  function(x, ...) {
    S4_to_list(x)
  }
)
