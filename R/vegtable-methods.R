# TODO:   Methods specific for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# Access to head "$" method ----------------------------------------------------
setMethod("$", signature(x="vegtable"), function(x, name) {
            return(x@head[[name]])
        }
)

setReplaceMethod("$", signature(x="vegtable"), function(x, name, value) {
            x@head[[name]] <- value 
            return(x) 
        }
)

# Access to head "[" method ----------------------------------------------------
setMethod("[", signature(x="vegtable"), function(x, i, j, ..., drop=FALSE) {
            if(missing(i)) i <- TRUE
            if(missing(j)) j <- TRUE
            # Resolving problems with NAs
            if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
            if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
            x@head <- x@head[i,j,drop]
            # Subset on samples
            x@samples <- x@samples[x@samples$RELEVE_NR %in% x@head$RELEVE_NR,]
            # Subset on species (same procedure as in import_vegtable)
            .UsageIDs <- list(UsageIDs=unique(x@samples$TaxonUsageID))
            attach(.UsageIDs)
            x@species <- subset(x@species, TaxonUsageID %in% UsageIDs)
            detach(.UsageIDs)
            # Subset on relations
            x@relations <- x@relations[sapply(sapply(x@relations, "colnames"), "[",
                            1) %in% colnames(x@head)]
            # Subset on syntax (not yet implemented)
            # Output
            return(x)
        }
)

setReplaceMethod("[", signature(x="vegtable"), function(x, i, j, value) {
            if(missing(i)) i <- TRUE
            if(missing(j)) j <- TRUE
            # Resolving problems with NAs
            if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
            if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
            x@head[i,j] <- value
            return(x)
        }
)

# head function ----------------------------------------------------------------
setMethod("head", signature(x="vegtable"), function(x, ...) {
            return(x@head)
        }
)

# Generic for replacement method
setGeneric("head<-", function(x, value)
            standardGeneric("head<-"))

# Replacement method
setReplaceMethod("head", signature(x="vegtable", value="data.frame"),
        function(x, value) {
            if(colnames(value)[1] != "RELEVE_NR")
                stop("First column in value have to be called 'RELEVE_NR'")
            rownames(value) <- paste(value$RELEVE_NR)
            if(!all(x@samples$RELEVE_NR %in% value$RELEVE_NR))
                stop("Some plots are missing in 'value'")
            x@head <- value[value$RELEVE_NR %in% x@samples$RELEVE_NR,]
            # Popups?
            return(x)
        }
)
