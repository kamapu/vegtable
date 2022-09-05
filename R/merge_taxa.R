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
