#' @name shaker-class
#' @aliases shaker
#'
#' @title Class containing Cocktail algorithms.
#'
#' @description
#' Objects used for collecting Cocktail definitions.
#'
#' These objects work as \bold{expert systems} for recognition of defined
#' vegetation units among plots of a [vegtable-class] object.
#' A `shaker` object will be always dependent on a [vegtable-class] object,
#' which is called `companion`.
#' Since modifications in the `companion` may affect the functionality of
#' the `shaker` object, it will be recommended to create the last during
#' a session by a source script instead of recycling them from old R images.
#'
#' @slot pseudos List containing IDs of taxa that will be merged into
#'     pseudo-species.
#' @slot groups List containing IDs of taxa belonging to the same Cocktail
#'     group.
#' @slot dominants A data frame including lists of species used as dominant
#'     species in Cocktail algorithms, as well as operators and cover values
#'     used in the formulas.
#' @slot formulas List with formulas that will be used as definitions for
#'     vegetation units.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [make_cocktail()] [set_pseudo()] [set_group()] [set_formula()]
#'
#' @examples
#' showClass("shaker")
#' @exportClass shaker
#'
setClass("shaker",
  slots = c(
    pseudos = "list",
    groups = "list",
    dominants = "data.frame",
    formulas = "list"
  ),
  prototype = list(
    pseudos = list(),
    groups = list(),
    dominants = data.frame(
      TaxonConceptID = integer(),
      operator = character(), value = numeric(),
      stringsAsFactors = FALSE
    ),
    formulas = list()
  ),
  validity = function(object) {
    if (any(duplicated(do.call(c, object@pseudos)))) {
      return("Some pseudo-species are sharing taxon concepts")
    }
    if (is.null(names(object@groups))) {
      return("Members of slot 'groups' have to be named")
    }
    if (any(duplicated(do.call(c, object@groups)))) {
      return("Some species groups are sharing taxon concepts")
    }
    if (any(!c("TaxonConceptID", "operator", "value") %in%
      colnames(object@dominants))) {
      return("Columns 'TaxonConceptID', 'operator', and 'value' are mandatory in slot 'dominants'")
    }
    if (any(duplicated(apply(object@dominants, 1, function(x) {
      gsub(" ", "", paste(x, collapse = " "),
        fixed = TRUE
      )
    })))) {
      return("Some rules in slot 'dominants' are duplicated")
    }
    if (is.null(names(object@formulas))) {
      return("Members of slot 'formulas' have to be named")
    }
    if (any(duplicated(do.call(c, object@formulas)))) {
      return("Some formulas are duplicated")
    }
  }
)
