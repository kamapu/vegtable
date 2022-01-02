#' @name coverconvert
#' @aliases coverconvert-class
#'
#' @title Cover conversion tables
#'
#' @description
#' Cover conversion tables for [vegtable-class] objects.
#'
#' This class implements conversions from different cover scales in percentage
#' cover. For transformations to percentage cover, the function [cover_trans()]
#' should be than used.
#'
#' @slot value List containing the levels of each scale.
#' @slot conversion List with the respective start and end cut levels for the
#' scale levels.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [tv2coverconvert()] [braun_blanquet].
#'
#' @examples
#' showClass("coverconvert")
#'
#' ## Add a custom scale
#' Scale <- new("coverconvert")
#' Scale$my_scale <- list(
#'   value = factor(c("low", "medium", "high"), levels = c("low", "medium", "high")),
#'   conversion = c(0, 50, 75, 100)
#' )
#' summary(Scale)
#' @exportClass coverconvert
#'
setClass("coverconvert",
  slots = c(
    value = "list",
    conversion = "list"
  ),
  prototype = list(
    value = list(),
    conversion = list()
  ),
  validity = function(object) {
    if (length(object@value) > 0) {
      # For whole object
      if (!all(names(object@value) == names(object@conversion))) {
        return("Names in slots 'value' and 'conversion' do not match")
      }
    }
    # For single scales
    for (i in names(object@value)) {
      if (length(object@value[[i]]) !=
        length(object@conversion[[i]]) - 1) {
        return(paste0(
          "Invalid length of vectors in scale '", i,
          "'"
        ))
      }
      if (!is.numeric(object@conversion[[i]])) {
        return(paste0(
          "Values of 'conversion' in scale '", i,
          "' have to be of class numeric"
        ))
      }
      if (!is.factor(object@value[[i]])) {
        return(paste0(
          "Values of 'value' in scale '", i,
          "' have to be of class factor"
        ))
      }
      if (!all(object@conversion[[i]] ==
        object@conversion[[i]][
          order(object@conversion[[i]])
        ])) {
        return(paste0(
          "Sort values of 'conversion' in scale '",
          i, "' in increasing order"
        ))
      }
    }
  }
)
