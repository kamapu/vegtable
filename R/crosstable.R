# TODO:   Function and methods to produce cross tables
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("crosstable", function(formula, data, ...)
            standardGeneric("crosstable"))

# Method for data frames
setMethod("crosstable", signature(formula="formula", data="data.frame"),
        function(formula, data, FUN, na_to_zero=FALSE, ...) {
            if(!all(c(as.character(formula)[2], attr(terms(formula),
                                            "term.labels")) %in%
                            colnames(data)))
                stop("all terms in 'formula' must be a column in 'data'")
            data <- aggregate(formula, data, FUN, ...)
            coverage <- as.character(formula)[2]
            plots <- attr(terms(formula), "term.labels")[1]
            # for multiple plot entries
            if(length(attr(terms(formula), "term.labels")) > 2) {
                data$.spp <- apply(data[attr(terms(formula),
                                        "term.labels")[-1]], 1, paste,
                        collapse=".")
                spp <- ".spp"
            } else spp <- attr(terms(formula), "term.labels")[2]
            cross <- expand.grid(unique(data[,spp]), unique(data[,plots]),
                    stringsAsFactors=FALSE)
            colnames(cross) <- c(spp,plots)
            cross[,coverage] <- data[match(paste(cross[,spp],cross[,plots]),
                            paste(data[,spp],data[,plots])), coverage]
            if(na_to_zero) cross[is.na(cross[,coverage]),coverage] <- 0
            cross <- matrix(cross[,coverage],
                    ncol=length(unique(cross[,plots])),
                    dimnames=list(unique(cross[,spp]), unique(cross[,plots])))
            # final output data frame
            if(spp == ".spp") {
                cross_margin <- unique(data[c(".spp", attr(terms(formula),
                                                "term.labels")[-1])])
                cross_margin <- cross_margin[match(rownames(cross),
                                cross_margin$.spp), colnames(cross_margin)[-1]]
            } else {
                cross_margin <- data.frame(unique(data[,spp]),
                        stringsAsFactors=FALSE)
                colnames(cross_margin) <- spp
            }
            cross <- do.call(cbind, list(cross_margin, cross))
            rownames(cross) <- NULL # reseting row names
            return(cross)
        }
)

# Method for vegtable objects
setMethod("crosstable", signature(formula="formula", data="vegtable"),
        function(formula, data, FUN, paste_author=FALSE, na_to_zero=FALSE,
                ...) {
            if(paste_author)
                data@species@taxonNames$TaxonName <- with(
                        data@species@taxonNames, paste(TaxonName, AuthorName))
            data@samples$TaxonConceptID <- data@species@taxonNames[
                    match(data@samples$TaxonUsageID,
                            data@species@taxonNames$TaxonUsageID),
                    "TaxonConceptID"]
            data@samples$TaxonName <- data@species@taxonNames[
                    match(data@samples$TaxonUsageID,
                            data@species@taxonNames$TaxonUsageID),
                    "TaxonName"]
            data@samples$AcceptedName <- data@species@taxonRelations[
                    match(data@samples$TaxonConceptID,
                            data@species@taxonRelations$TaxonConceptID),
                    "AcceptedName"]
            data@samples$AcceptedName <- data@species@taxonNames[
                    match(data@samples$AcceptedName,
                            data@species@taxonNames$TaxonUsageID),
                    "TaxonName"]
            for(i in colnames(data@header))
                data@samples[,i] <- data@header[match(data@samples$ReleveID,
                                data@header$ReleveID),i]
            if(length(attr(terms(formula), "term.labels")) == 1)
                return(aggregate(formula, data@samples, FUN, ...)) else
                return(crosstable(formula, data@samples, FUN, na_to_zero, ...))
        }
)
