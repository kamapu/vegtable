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
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @examples
#' ## Plots containing Podocarpus observations
#' Kenya_veg@species <- subset(Kenya_veg@species, grepl("Podocarpus", TaxonName),
#'   slot = "names"
#' )
#'
#' Kenya_veg <- subset(Kenya_veg, TaxonUsageID %in%
#'   Kenya_veg@species@taxonNames$TaxonUsageID, slot = "samples")
#' \dontrun{
#' vegtable2kml(Kenya_veg, "Podocarpus.kml")
#' }
#'
#' @name vegtable2kml
#'
#' @rdname vegtable2kml
#'
#' @seealso [vegtable-deprecated]
#'
#' @keywords internal
#'
NULL

#' @rdname vegtable2kml
#' @export
#'
vegtable2kml <- function(obj, ...) {
  .Deprecated(msg = paste(
    "This function is deprecated.",
    "Visit the package's site for alternative mapping methods."
  ))
}
