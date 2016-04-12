# TODO:   subset functions for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

subset_by_species <- function(vegtable, ...) {
    if(class(vegtable) != "vegtable")
        stop("'vegtable' should be an object of class vegtable.")
    # First subset species list
    if(class(vegtable@species) == "taxlist") {
        vegtable@species <- subset(vegtable@species, ...)
        # Subset on observations
        vegtable@samples <- vegtable@samples[vegtable@samples$TaxonUsageID %in%
                        vegtable@species@taxonNames$TaxonUsageID,]
    }
    # Subset on heads
    vegtable@head <- vegtable@head[vegtable@head$RELEVE_NR %in%
                    vegtable@samples$RELEVE_NR,]
    vegtable@head <- vegtable@head[,!apply(vegtable@head, 2,
                    function(x) all(is.na(x)))]
    # Subset on popups
    vegtable@popups <- vegtable@popups[sapply(sapply(vegtable@popups,
                            "colnames"), "[", 1) %in% colnames(vegtable@head)]
    # Subset on syntax (not yet implemented)
    # Output
    return(vegtable)
}

# subset_by_head
# subset_by_popup
