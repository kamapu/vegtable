#' @name vegtable-deprecated
#' @title Deprecated functions in package \pkg{vegtable}
#' @description
#'   The functions listed below are deprecated and will be defuncti in the near
#'   future.
#' @name vegtable-deprecated
#' @rdname vegtable-deprecated
#' @keywords internal
NULL

#' @rdname vegtable-deprecated
#'
#' @title Search matchings between character and taxlist objects.
#'
#' @description
#' Names provided in a character vector will be compared with names stored in
#' slot `taxonNames` of an object of class [taxlist-class] by using the
#' function [stringdist::stringsim()].
#'
#' This method is applied to the slot `species` in the input [vegtable-class]
#' object.
#'
#' @param x A character vector with names to be compared.
#' @param object An object of class [vegtable-class] to be compared with.
#' @param ... Further arguments passed to [taxlist::match_names()].
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.de}
#'
#' @seealso [vegtable-deprecated]
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

#' @name vegtable-deprecated
#'
#' @title Merge concepts
#'
#' @description
#' Merge taxon concepts form into single ones or insert accepted names to slot
#' samples.
#'
#' This method is applied to a function defined in the package [taxlist-package]
#' and only modify the slot `species` in the input `object`.
#'
#' The use of `taxa2samples()` with `merge_to` argument will produce a similar
#' result as using `merge_taxa` with `level` argument, but `taxa2samples()`
#' will replace the records in slot samples by the respective accepted names
#' without any modification in slot species.
#' Additionally taxon concept IDs will be addes as columns in samples and taxon
#' traits if indicated in argument `add_traits`.
#'
#' @param object Object of class [vegtable-class].
#' @param concepts Numeric (integer) vector including taxon concepts to be
#'     merged.
#' @param level Character value indicating the level to which the taxa
#'     have to be merged.
#' @param ... Further arguments passed to [taxlist::merge_taxa()].
#'
#' @return An object of class [vegtable-class].
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.de}
#'
#' @keywords internal
NULL

#' @rdname vegtable-deprecated
#' @section `merge_taxa`:
#' For `merge_taxa(object, ...)`, use `merge_taxa(object@@species, ...)`
#' @export
merge_taxa <- function(object, ...) {
  .Deprecated(msg = paste(
    "This function is deprecated.",
    "Use 'merge_taxa(object@species)' instead."
  ))
}

#' @rdname vegtable-deprecated
#'
#' @title Convert cover scales to percent cover
#'
#' @description
#' Convert values of a categorical cover scale to percentage values.
#'
#' This function requires as input a [coverconvert-class] object which contains
#' the conversion tables.
#'
#' In the case of [vegtable-class] objects, the conversion is already embedded
#' in the slot `coverconvert`.
#'
#' Three rules are implemented for transformation, either `top` (values
#' transformed to the top of the range), `middle` (transformation at the
#' midpoint), and `bottom` (conversion at the lowest value of the
#' range). In the later case, transformation ranges starting at 0% of cover
#' can be set to a different value by the argument `zeroto`.
#'
#' When `replace=FALSE`, existing values of cover in the [vegtable-class]
#' object will be maintained. Since there is not a standard naming of cover
#' values, in the transformation the name of cover variable should be
#' indicated in the argument `to`.
#'
#' @param _data,x Either a factor or character vector, or a [vegtable-class] object.
#' @param conversion An object of class [vegtable-class].
#' @param from Scale name of values in `x` as character value.
#' @param to Name of the column in slot `samples` for writing converted values.
#' @param replace Logical value indicating whether existing cover values should
#'     be replaced or not.
#' @param rule Rule applied for the conversion (see details).
#' @param zeroto Value used to replace levels with bottom at 0% cover.
#' @param ... Further arguments passed from or to other methods.
#'
#' @return Either a vector or a [vegtable-class] object.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [vegtable-deprecated]
#'
#' @rdname transform
#'
#' @keywords internal
#'
NULL

#' @rdname vegtable-deprecated
#'
#' @export
#'
transform <- function(x, ...) {
  .Deprecated(new = "cover_trans")
}

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
