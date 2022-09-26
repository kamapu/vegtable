#' @name vegtable-deprecated
#' @title Deprecated functions in package \pkg{vegtable}
#' @description
#' The functions listed below are deprecated and will be defuncti in the near
#' future.
#' @name vegtable-deprecated
#' @rdname vegtable-deprecated
#' @keywords internal
NULL

#' @rdname vegtable-deprecated
#' @section `match_names`:
#' For `match_names(object, ...)`, use `match_names(object@@species, ...)`
setMethod(
  "match_names", signature(x = "character", object = "vegtable"),
  function(x, object, ...) {
    .Deprecated(msg = paste(
      "This function is deprecated.",
      "Use 'match_names(object@species, ...)' instead."
    ))
  }
)

#' @rdname vegtable-deprecated
#' @section `merge_taxa`:
#' For `merge_taxa(object, ...)`, use `merge_taxa(object@@species, ...)`
#' @export
setMethod(
  "merge_taxa", signature(
    object = "vegtable", concepts = "numeric",
    level = "missing"
  ),
  function(object, concepts, level, ...) {
    .Deprecated(msg = paste(
      "This function is deprecated.",
      "Use 'merge_taxa(object@species, ...)' instead."
    ))
  }
)

#' @rdname vegtable-deprecated
#' @section `vegtable2kml`:
#' Use leaflet to map plot observations.
vegtable2kml <- function(obj, ...) {
  .Deprecated(msg = paste(
    "This function is deprecated.",
    "Visit the package's site for alternative mapping methods using leaflet."
  ))
}
