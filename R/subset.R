# TODO:   subset functions for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# subset_by_species ------------------------------------------------------------
subset_by_species <- function(vegtable, ...) {
    if(class(vegtable) != "vegtable")
        stop("'vegtable' should be an object of class vegtable.")
    # First subset species list
    if(class(vegtable@species) == "taxlist") {
        vegtable@species <- subset(vegtable@species, ...)
        # Subset on samples
        vegtable@samples <- vegtable@samples[vegtable@samples$TaxonUsageID %in%
                        vegtable@species@taxonNames$TaxonUsageID,]
    }
    # Subset on heads
    vegtable@head <- vegtable@head[vegtable@head$RELEVE_NR %in%
                    vegtable@samples$RELEVE_NR,]
    vegtable@head <- vegtable@head[,!apply(vegtable@head, 2,
                    function(x) all(is.na(x)))]
    # Subset on relations
    vegtable@relations <- vegtable@relations[sapply(sapply(vegtable@relations,
                            "colnames"), "[", 1) %in% colnames(vegtable@head)]
    # Subset on syntax (not yet implemented)
    # Output
    return(vegtable)
}

# subset_by_head ---------------------------------------------------------------
subset_by_head <- function(vegtable, ...) {
    if(class(vegtable) != "vegtable")
        stop("'vegtable' should be an object of class vegtable.")
    # First subset head
    vegtable@head <- subset(vegtable@head, ...)
    # Subset on samples
    vegtable@samples <- vegtable@samples[vegtable@samples$RELEVE_NR %in%
                    vegtable@head$RELEVE_NR,]
    # Subset on species (same procedure as in import_vegtable)
    .UsageIDs <- list(UsageIDs=unique(vegtable@samples$TaxonUsageID))
    attach(.UsageIDs)
    vegtable@species <- subset(vegtable@species, TaxonUsageID %in% UsageIDs)
    detach(.UsageIDs)
    # Subset on relations
    vegtable@relations <- vegtable@relations[sapply(sapply(vegtable@relations,
                            "colnames"), "[", 1) %in% colnames(vegtable@head)]
    # Subset on syntax (not yet implemented)
    # Output
    return(vegtable)
}

# subset_by_popup --------------------------------------------------------------
subset_by_popup <- function(vegtable, popup, ...) {
    if(class(vegtable) != "vegtable")
        stop("'vegtable' should be an object of class vegtable.")
    if(!popup %in% names(vegtable@relations))
        stop("The requested 'popup' is not existing in 'vegtable'.")
    # Subset on popup
    vegtable@relations[[popup]] <- subset(vegtable@relations[[popup]], ...)
    vegtable@relations[[popup]][,1] <- factor(vegtable@relations[[popup]][,1])
    popvar <- colnames(vegtable@relations[[popup]])[1]
    # Subset on head
    vegtable@head <- vegtable@head[paste(vegtable@head[,popvar]) %in%
                    levels(vegtable@relations[[popup]][,popvar]),]
    vegtable@head[,popvar] <- factor(paste(vegtable@head[,popvar]),
            levels=levels(vegtable@relations[[popup]][,popvar]))
    # Subset on samples
    vegtable@samples <- vegtable@samples[vegtable@samples$RELEVE_NR %in%
                    vegtable@head$RELEVE_NR,]
    # Subset on species (same procedure as in import_vegtable)
    .UsageIDs <- list(UsageIDs=unique(vegtable@samples$TaxonUsageID))
    attach(.UsageIDs)
    vegtable@species <- subset(vegtable@species, TaxonUsageID %in% UsageIDs)
    detach(.UsageIDs)
    # Subset on syntax (not yet implemented)
    # Output
    return(vegtable)
}

# subset_by_syntax -------------------------------------------------------------
