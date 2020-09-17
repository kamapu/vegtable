#' @name used_synonyms
#' 
#' @title Retrieve synonyms or taxon concepts used in a data set
#' 
#' @description 
#' Plots records are rather linked to plant names than plant taxon concepts.
#' The function `used_synonyms()` provides a quick report about synonyms used in
#' a data set (a [vegtable-class] object) and their respective accepted names.
#' 
#' Additionally, not all taxon concepts included in the taxonomic list (slot
#' **species**) may be recorded in the plot observations.
#' In that case the function `used_concepts()` will optimize the size of the
#' taxonomic list by discarding taxa that are not "in use".
#' Alternatively parents or children of these taxa may be included in the output
#' data set. 
#' 
#' @param x A [vegtable-class] object.
#' @param keep_children Argument passed to [taxlist::get_children()].
#' @param keep_parents Argument passed to [taxlist::get_parents()].
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

#' @rdname used_synonyms
#' 
#' @exportMethod used_concepts
#' 
setGeneric("used_concepts",
		function(x, ...)
			standardGeneric("used_concepts")
)

#' @rdname used_synonyms
#' 
#' @aliases used_concepts used_concepts,vegtable-method
#' 
setMethod("used_concepts", signature(x="vegtable"),
		function(x, keep_children=FALSE, keep_parents=FALSE, ...) {
			concepts <- unique(x@species@taxonNames$TaxonConceptID[
							x@species@taxonNames$TaxonUsageID %in%
									x@samples$TaxonUsageID])
			z <- x@species
			z@taxonRelations <- z@taxonRelations[
					z@taxonRelations$TaxonConceptID %in% concepts,]
			z <- clean(z)
			if(keep_children)
				z <- get_children(x@species, z)
			if(keep_parents)
				z <- get_parents(x@species, z)
			return(z)
		})
