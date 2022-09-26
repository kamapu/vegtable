#' @name as
#' @rdname coerce-methods
#'
#' @title Coerce objects to lists
#'
#' @description
#' Coerce vegtable objects to a list with every slot as a component of the list.
#' This way allows to explore content and solve problems when validity checks
#' fail.
#'
#' Coercion is applied for different classes by vegtable.
#'
#' @param x,object An object to be coerced.
#' @param Class A character value with the target class (not used).
#' @param value A character value indicating the class to be coerced to. This is
#'     only required by the replacement methods.
#' @param ... further arguments passed from or to other methods.
#'
#' @return An object of class `list`.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## vegtable as list
#' veg <- as(Kenya_veg, "list")
#' names(veg)
#'
#' ## coverconvert as list
#' as(Kenya_veg@coverconvert, "list")
#'
#' @aliases as.list,vegtable-method coerce,vegtable,list-method
#' coerce,coverconvert,list-method
#'
#' @exportMethod as.list
setMethod(
  "as.list", signature(x = "vegtable"),
  function(x, ...) {
    S4_to_list(x)
  }
)

setAs("vegtable", "list", function(from) as.list(from))

#' @rdname coerce-methods
#' @aliases as<-,vegtable,missing,character-method
setReplaceMethod(
  "as", signature(object = "vegtable", Class = "missing", value = "character"),
  function(object, Class, value) {
    return(as(object, value))
  }
)

#' @rdname coerce-methods
#' @aliases as.list,coverconvert-method
setMethod(
  "as.list", signature(x = "coverconvert"),
  function(x, ...) {
    # z <- S4_to_list(x)
    z <- list()
    for (i in names(x)) {
      z[[i]] <- data.frame(
        value = x@value[[i]],
        bottom = x@conversion[[i]][-(length(x@conversion[[i]]))],
        top = x@conversion[[i]][-1]
      )
      for (j in seq_along(z[[i]]$value)) {
        if (z[[i]]$bottom[j] == z[[i]]$top[j]) {
          z[[i]]$bottom[j] <- z[[i]]$bottom[j - 1]
        }
      }
    }
    return(z)
  }
)

setAs("coverconvert", "list", function(from) as.list(from))

#' @rdname coerce-methods
#' @aliases as<-,coverconvert,missing,character-method
setReplaceMethod(
  "as", signature(
    object = "coverconvert", Class = "missing",
    value = "character"
  ),
  function(object, Class, value) {
    return(as(object, value))
  }
)
