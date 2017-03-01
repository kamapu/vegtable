# TODO:   A clean method for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# Method for 'vegtable' object
setMethod("clean", signature(object="vegtable"),
        function(object, ...) {
            # clean slot samples
            object@samples <- object@samples[object@samples$ReleveID %in%
                            object@header$ReleveID,]
            # clean species
            UsageID <- unique(object@samples$TaxonUsageID)
            ConceptID <- unique(object@species@taxonNames[
                            object@species@taxonNames$TaxonUsageID %in% UsageID,
                            "TaxonConceptID"])
            object@species@taxonRelations <- object@species@taxonRelations[
                    object@species@taxonRelations$TaxonConceptID %in%
                            ConceptID,]
            object@species <- clean(object@species)
            # clean relations
            object@relations <- object@relations[names(object@relations) %in%
                            colnames(object@header)]
            # output object
            return(object)
        }
)
