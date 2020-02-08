#' @name used_synonyms
#' 
#' @title Retrieve synonyms used in the data set
#' 
#' @description 
#' Plots records are rather linked to plant names than plant taxon concepts.
#' This function provides a quick report about synonyms used in a data set (a
#' [vegtable-class] object) and their respective accepted name.
#' 
#' This function will only retrieve synonyms that are used in plot records.
#' 
#' @param x A [vegtable-class] object.
#' @param ... Further arguments to be passed from or to another methods.
#' 
#' @return
#' A data frame with following columns:
#' \describe{
#'   \item{SynonymsID}{Usage ID of synonyms.}
#'   \item{Synonym}{The synonym itself.}
#'   \item{SynonymAuthor}{Author of synonym.}
#'   \item{TaxonConceptID}{ID of the taxon concept.}
#'   \item{AcceptedNameID}{Usage ID of the accepted name.}
#'   \item{AcceptedName}{The respective accepted name.}
#'   \item{AcceptedNameAuthor}{The author of the accepted name.}
#' }
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
#' @rdname used_synonyms
#' 
#' @exportMethod used_synonyms
#' 
setGeneric("used_synonyms",
        function(x, ...)
            standardGeneric("used_synonyms")
)

#' @rdname used_synonyms
#' 
#' @aliases used_synonyms,vegtable-method
#' 
setMethod("used_synonyms", signature(x="vegtable"),
        function(x, ...) {
            SYN <- x@samples$TaxonUsageID[!x@samples$TaxonUsageID %in%
                            x@species@taxonRelations$AcceptedName]
            SYN <- data.frame(SynonymID=unique(SYN), stringsAsFactors=FALSE)
            SYN$Synonym <- x@species@taxonNames$TaxonName[match(SYN$SynonymID,
                            x@species@taxonNames$TaxonUsageID)]
            SYN$SynonymAuthor <- x@species@taxonNames$AuthorName[
                    match(SYN$SynonymID, x@species@taxonNames$TaxonUsageID)]
            SYN$TaxonConceptID <- x@species@taxonNames$TaxonConceptID[
                    match(SYN$SynonymID, x@species@taxonNames$TaxonUsageID)]
            SYN$AcceptedNameID <- x@species@taxonRelations$AcceptedName[
                    match(SYN$TaxonConceptID,
                            x@species@taxonRelations$TaxonConceptID)]
            SYN$AcceptedName <- x@species@taxonNames$TaxonName[
                    match(SYN$AcceptedNameID,
                            x@species@taxonNames$TaxonUsageID)]
            SYN$AcceptedNameAuthor <- x@species@taxonNames$AuthorName[
                    match(SYN$AcceptedNameID,
                            x@species@taxonNames$TaxonUsageID)]
            return(SYN)
        }
)
