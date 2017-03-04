# TODO:   A clean method for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# Method for 'vegtable' object
setMethod("clean", signature(object="vegtable"),
        function(object, ...) {
            # clean slot species
            object@species <- clean(object@species)
            # compare samples and header
            ReleveID <- intersect(object@header$ReleveID,
                    object@samples$ReleveID)
            object@header <- object@header[object@header$ReleveID %in%
                            ReleveID,]
            object@samples <- object@samples[object@samples$ReleveID %in%
                            ReleveID,]
            # compare species and samples
            UsageID <- intersect(object@samples$TaxonUsageID,
                    object@species@taxonNames$TaxonUsageID)
            ConceptID <- unique(object@species@taxonNames[
                            object@species@taxonNames$TaxonUsageID %in% UsageID,
                            "TaxonConceptID"])
            object@species@taxonRelations <- object@species@taxonRelations[
                    object@species@taxonRelations$TaxonConceptID %in%
                            ConceptID,]
            object@species <- clean(object@species)
            object@samples <- object@samples[
                    object@species@taxonNames$TaxonUsageID %in% UsageID,]
            # compare samples and header AGAIN
            ReleveID <- intersect(object@header$ReleveID,
                    object@samples$ReleveID)
            object@header <- object@header[object@header$ReleveID %in%
                            ReleveID,]
            object@samples <- object@samples[object@samples$ReleveID %in%
                            ReleveID,]
            # output object
            return(object)
        }
)
