#' @name match_names
#' @aliases match_names,character,vegtable-method
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
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [taxlist::match_names()] [stringdist::stringsim()]
#'
#' @exportMethod match_names
#'
setMethod(
  "match_names", signature(x = "character", object = "vegtable"),
  function(x, object, ...) {
    new_names <- match_names(x, object@species, ...)
    return(new_names)
  }
)
