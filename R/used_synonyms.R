#' @name used_synonyms
#'
#' @title Retrieve synonyms or taxon concepts used in a data set
#'
#' @description
#' Plots records are rather linked to plant names than plant taxon concepts and
#' `used_synonyms()` lists all synonyms linked to records in a [vegtable-class]
#' object, including their respective accepted names.
#'
#' On the other side, the function `used_concepts()` produces a subset of the
#' taxonomic list embeded in the slot **species** including only taxonomic
#' concepts linked to records in the slot **samples**.
#'
#' @param x A [vegtable-class] object.
#' @param keep_children A logical argument indicating whether children of
#'     selected taxa should be included in the output or not.
#'     This argument passed to [get_children()].
#' @param keep_parents A logical argument indicating whether parents of
#'     selected taxa should be included in the output or not.
#'     This argument passed to [get_parents()].
#' @param ... Further arguments to be passed from or to another methods.
#'
#' @return
#' The function `used_synonyms()` returns a data frame including following
#' variables:
#' \describe{
#'   \item{SynonymID}{ID of the taxon usage name applied as synonym.}
#'   \item{Synonym}{The synonym itself.}
#'   \item{SynonymAuthor}{Author of synonym.}
#'   \item{TaxonConceptID}{ID of the respective taxon concept.}
#'   \item{AcceptedNameID}{ID of the taxon usage name set as accepted name of
#'       the taxon concept.}
#'   \item{AcceptedName}{The respective accepted name.}
#'   \item{AcceptedNameAuthor}{The author of the accepted name.}
#' }
#'
#' The function `used_concepts()` returns a [taxlist-class] object including
#' only taxa occurring in the plot observations of the input [vegtable-class]
#' object.
#'
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#'
#' @seealso [accepted_name()]
#'
#' @examples
#' ## Synonyms used in the Kenya_veg
#' Synonyms <- used_synonyms(Kenya_veg)
#' head(Synonyms)
#'
#' ## Subset species list to used concepts
#' species <- used_concepts(Kenya_veg)
#' Kenya_veg@species
#' species
#'
#' @rdname used_synonyms
#' @export
used_synonyms <- function(x, ...) {
  UseMethod("used_synonyms", x)
}

#' @rdname used_synonyms
#' @aliases used_synonyms,vegtable-method
#' @method used_synonyms vegtable
#' @export
used_synonyms.vegtable <- function(x, ...) {
  SYN <- x@samples$TaxonUsageID[!x@samples$TaxonUsageID %in%
    x@species@taxonRelations$AcceptedName]
  SYN <- data.frame(SynonymID = unique(SYN), stringsAsFactors = FALSE)
  SYN$Synonym <- x@species@taxonNames$TaxonName[match(
    SYN$SynonymID,
    x@species@taxonNames$TaxonUsageID
  )]
  SYN$SynonymAuthor <- x@species@taxonNames$AuthorName[
    match(SYN$SynonymID, x@species@taxonNames$TaxonUsageID)
  ]
  SYN$TaxonConceptID <- x@species@taxonNames$TaxonConceptID[
    match(SYN$SynonymID, x@species@taxonNames$TaxonUsageID)
  ]
  SYN$AcceptedNameID <- x@species@taxonRelations$AcceptedName[
    match(
      SYN$TaxonConceptID,
      x@species@taxonRelations$TaxonConceptID
    )
  ]
  SYN$AcceptedName <- x@species@taxonNames$TaxonName[
    match(
      SYN$AcceptedNameID,
      x@species@taxonNames$TaxonUsageID
    )
  ]
  SYN$AcceptedNameAuthor <- x@species@taxonNames$AuthorName[
    match(
      SYN$AcceptedNameID,
      x@species@taxonNames$TaxonUsageID
    )
  ]
  return(SYN)
}

#' @rdname used_synonyms
#' @export
used_concepts <- function(x, ...) {
  UseMethod("used_concepts", x)
}

#' @rdname used_synonyms
#' @aliases used_concepts,vegtable-method
#' @method used_concepts vegtable
#' @export
used_concepts.vegtable <- function(x, keep_children = FALSE,
                                   keep_parents = FALSE, ...) {
  concepts <- unique(x@species@taxonNames$TaxonConceptID[
    x@species@taxonNames$TaxonUsageID %in%
      x@samples$TaxonUsageID
  ])
  z <- x@species
  z@taxonRelations <- z@taxonRelations[
    z@taxonRelations$TaxonConceptID %in% concepts,
  ]
  z <- clean(z)
  if (keep_children) {
    z <- get_children(x@species, z)
  }
  if (keep_parents) {
    z <- get_parents(x@species, z)
  }
  return(z)
}
