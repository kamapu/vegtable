# TODO:   Retrieving synonyms used in a data set with respective acc. names
# 
# Author: Miguel Alvarez
################################################################################

setGeneric("used_synonyms",
        function(x, ...)
            standardGeneric("used_synonyms")
)

# Method for vegtable objects
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
