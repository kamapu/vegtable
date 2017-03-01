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
    # Subset on headers
    vegtable@header <- vegtable@header[vegtable@header$RELEVE_NR %in%
                    vegtable@samples$RELEVE_NR,]
    vegtable@header <- vegtable@header[,!apply(vegtable@header, 2,
                    function(x) all(is.na(x)))]
    # Subset on relations
    vegtable@relations <- vegtable@relations[sapply(sapply(vegtable@relations,
                            "colnames"), "[", 1) %in% colnames(vegtable@header)]
    # Subset on syntax (not yet implemented)
    # Output
    return(vegtable)
}

# subset_by_header ---------------------------------------------------------------
subset_by_header <- function(vegtable, ...) {
    if(class(vegtable) != "vegtable")
        stop("'vegtable' should be an object of class vegtable.")
    # First subset header
    vegtable@header <- subset(vegtable@header, ...)
    # Subset on samples
    vegtable@samples <- vegtable@samples[vegtable@samples$RELEVE_NR %in%
                    vegtable@header$RELEVE_NR,]
    # Subset on species (same procedure as in import_vegtable)
    .UsageIDs <- list(UsageIDs=unique(vegtable@samples$TaxonUsageID))
    attach(.UsageIDs)
    vegtable@species <- subset(vegtable@species, TaxonUsageID %in% UsageIDs)
    detach(.UsageIDs)
    # Subset on relations
    vegtable@relations <- vegtable@relations[sapply(sapply(vegtable@relations,
                            "colnames"), "[", 1) %in% colnames(vegtable@header)]
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
    # Subset on header
    vegtable@header <- vegtable@header[paste(vegtable@header[,popvar]) %in%
                    levels(vegtable@relations[[popup]][,popvar]),]
    vegtable@header[,popvar] <- factor(paste(vegtable@header[,popvar]),
            levels=levels(vegtable@relations[[popup]][,popvar]))
    # Subset on samples
    vegtable@samples <- vegtable@samples[vegtable@samples$RELEVE_NR %in%
                    vegtable@header$RELEVE_NR,]
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
