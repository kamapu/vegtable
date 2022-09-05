#' @title Mapping of plot observations
#'
#' @description
#' This function is producing and displaying KML files.
#'
#' Georeferenced plots can be quickly displayed in
#' [Google Earth](https://www.google.com/intl/en_us/earth/) using this function.
#'
#' @param obj Input object containing coordinate values.
#' @param file Character value with the name of output file (including file
#'     extension).
#' @param coords Either a character vector or a formula indicating the names of
#'     coordinate values.
#' @param srs Spatial reference system as `proj4string`.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' A KML file, which will be automatically opened in **Google Earth**.
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.de}
#'
#' @name vegtable-deprecated
#'
#' @seealso [vegtable-deprecated]
#'
#' @keywords internal
NULL

#' @rdname vegtable-deprecated
#' @export
vegtable2kml <- function(obj, ...) {
  .Deprecated(msg = paste(
    "This function is deprecated.",
    "Visit the package's site for alternative mapping methods using leaflet."
  ))
}
