# TODO:   Methods specific for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# Access to header by "$"
setMethod("$", signature(x="vegtable"), function(x, name) {
            return(x@header[[name]])
        }
)

setReplaceMethod("$", signature(x="vegtable"), function(x, name, value) {
            x@header[[name]] <- value 
            return(x) 
        }
)

# Access to header by "["
setMethod("[", signature(x="vegtable"), function(x, i, j, ..., drop=FALSE) {
            if(missing(i)) i <- TRUE
            if(missing(j)) j <- TRUE
            # Resolving problems with NAs
            if(is.logical(i)) i[is.na(i)] <- FALSE else i <- na.omit(i)
            if(is.logical(j)) i[is.na(j)] <- FALSE else j <- na.omit(j)
            x@header <- x@header[i,j,drop]
            # Subset on samples
            x@samples <- x@samples[x@samples$ReleveID %in% x@header$ReleveID,]
            # Subset on species (same procedure as in import_vegtable)
            x <- clean(x)
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
            x@header[i,j] <- value
            return(x)
        }
)