# TODO:   Function and methods to produce cross tables
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("crosstable", function(formula, data, ...)
            standardGeneric("crosstable"))

# Method for data frames
setMethod("crosstable", signature(formula="formula", data="data.frame"),
        function(formula, data, FUN, na_to_zero=FALSE, use_nas=TRUE,
				as_matrix=FALSE, ...) {
            if(!all(c(as.character(formula)[2], attr(terms(formula),
                                            "term.labels")) %in%
                            colnames(data)))
                stop("all terms in 'formula' must be a column in 'data'")
			if(use_nas) {
				Terms <- c(as.character(formula)[2], attr(terms(formula),
								"term.labels"))
				for(i in Terms[-1]) {
					if(is.factor(data[,i]))
						data[,i] <- paste(data[,i])
					if(is.character(data[,i]))
						data[is.na(data[,i]),i] <- ""
				}
			}
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
			if(!as_matrix) {
				cross <- do.call(cbind, list(cross_margin, cross))
				rownames(cross) <- NULL # reseting row names
			}
            return(cross)
        }
)

# Method for vegtable objects
setMethod("crosstable", signature(formula="formula", data="vegtable"),
        function(formula, data, FUN, na_to_zero=FALSE, use_nas=TRUE, ...) {
            Terms <- c(as.character(formula)[2], attr(terms(formula),
                            "term.labels"))
            data@samples <- data@samples[,colnames(data@samples) %in%
                            c("ReleveID", "TaxonUsageID", Terms)]
            # Data from species
            data@samples$TaxonConceptID <- data@species@taxonNames[
                    match(data@samples$TaxonUsageID,
                            data@species@taxonNames$TaxonUsageID),
                    "TaxonConceptID"]
            if("TaxonName" %in% Terms & "AcceptedName" %in% Terms)
                stop("Terms 'TaxonName' and 'AcceptedName' are mutually exclusive in 'formula'")
            # 1: when usage name requested
            if("TaxonName" %in% Terms) {
                data@samples$TaxonName <- data@species@taxonNames[
                        match(data@samples$TaxonUsageID,
                                data@species@taxonNames$TaxonUsageID),
                        "TaxonName"]
                data@samples$AuthorName <- data@species@taxonNames[
                        match(data@samples$TaxonUsageID,
                                data@species@taxonNames$TaxonUsageID),
                        "AuthorName"]
            }
            # 2: when accepted name requested
            if("AcceptedName" %in% Terms) {
                data@samples$AcceptedName <- data@species@taxonRelations[
                        match(data@samples$TaxonConceptID,
                                data@species@taxonRelations$TaxonConceptID),
                        "AcceptedName"]
                data@samples$AuthorName <- data@species@taxonNames[
                        match(data@samples$AcceptedName,
                                data@species@taxonNames$TaxonUsageID),
                        "AuthorName"]
                data@samples$AcceptedName <- data@species@taxonNames[
                        match(data@samples$AcceptedName,
                                data@species@taxonNames$TaxonUsageID),
                        "TaxonName"]
            }
			# Data from traits (only for Accepted Name or TaxonConceptID)
			traits_names <- colnames(data@species@taxonTraits)[
					colnames(data@species@taxonTraits) != "TaxonConceptID"]
			if(any(Terms %in% traits_names)) {
				traits_names <- traits_names[traits_names %in% Terms]
				for(i in traits_names)
					data@samples[,i] <- data@species@taxonTraits[
							match(data@samples$TaxonConceptID,
									data@species@taxonTraits$TaxonConceptID),i]
			}
            # Data from header
            header_names <- colnames(data@header)[colnames(data@header) !=
                            "ReleveID"]
            if(any(header_names %in% Terms)) {
                header_names <- header_names[header_names %in% Terms]
                for(i in header_names)
                    data@samples[,i] <- data@header[match(data@samples$ReleveID,
                                    data@header$ReleveID), i]
            }
            # Continue with method data.frame
			return(crosstable(formula, data@samples, FUN, na_to_zero, use_nas,
							...))
        }
)
