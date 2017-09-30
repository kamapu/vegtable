# TODO:   A clean method for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# First a general function
clean_once <- function(object) {
    # compare samples and header
    ReleveID <- intersect(object@header$ReleveID,
            object@samples$ReleveID)
    object@header <- object@header[object@header$ReleveID %in%
                    ReleveID,]
    object@samples <- object@samples[object@samples$ReleveID %in%
                    ReleveID,]
    # compare species and samples
	## UsageID <- intersect(object@samples$TaxonUsageID,
	##         object@species@taxonNames$TaxonUsageID)
	## ConceptID <- unique(object@species@taxonNames[
	##                 object@species@taxonNames$TaxonUsageID %in% UsageID,
	##                 "TaxonConceptID"])
	## object@species@taxonRelations <- object@species@taxonRelations[
	##         object@species@taxonRelations$TaxonConceptID %in%
	##                 ConceptID,]
	## object@species <- clean(object@species)
    object@samples <- object@samples[object@samples$TaxonUsageID %in%
                    object@species@taxonNames$TaxonUsageID,]
    # delete header variables without data
    object@header <- object@header[,!apply(object@header, 2,
                    function(x) all(is.na(x)))]
    # delete samples variables without data
    object@samples <- object@samples[,!apply(object@samples, 2,
                    function(x) all(is.na(x)))]
    # delete orphaned relations
    object@relations <- object@relations[names(object@relations) %in%
                    colnames(object@header)]
    # delete orphaned cover conversions
    object@coverconvert@value <- object@coverconvert@value[
            names(object@coverconvert@value) %in% colnames(object@samples)]
    object@coverconvert@conversion <- object@coverconvert@conversion[
            names(object@coverconvert@conversion) %in%
                    names(object@coverconvert@value)]
    # output
    return(object)
}

# Method for 'vegtable' object
setMethod("clean", signature(object="vegtable"),
        function(object, times=2, ...) {
            count <- 0
            repeat {
                count <- count + 1
                object <- clean_once(object)
                if(count == times) break
            }
            return(object)
        }
)
