# TODO:   Add releves from a data frame into a vegtable object
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("add_releves",
		function(vegtable, releves, ...)
			standardGeneric("add_releves")
)

# Method for vegtable and data frame
setMethod("add_releves", signature(vegtable="vegtable", releves="data.frame"),
		function(vegtable, releves, header, abundance, split_string,
				usage_ids=FALSE, layers=FALSE, layers_var, format="crosstable",
				...) {
			# Step 1: Make database list, if necessary
			format <- pmatch(tolower(format), c("crosstable","databaselist"))
			if(!format %in% c(1:2))
				stop("Non valid value for 'format'.")
			if(format == 1) {
				releves <- cross2db(releves, layers, ...)
				colnames(releves)[1:2] <- c("ReleveID", "TaxonUsageID")
			}
			message(paste("Imported records:", nrow(releves)))
			# Step 1: Recognize species
			if(!usage_ids & format == 1)
				releves$TaxonUsageID <- match_names(releves$TaxonUsageID,
						vegtable)$TaxonUsageID
			message(paste("Matched taxon usage names:",
							length(unique(releves$TaxonUsageID))))
			# Step 3: Check layers, if necessary
			if(layers) {
				colnames(releves)[3] <- layers_var
				if(!layers_var %in% colnames(vegtable@samples))
					stop("Variable 'layers_var' not occurring in 'vegtable'")
				if(is.factor(vegtable@samples[,layers_var]))
					releves[,layers_var] <- factor(paste(releves[,layers_var]),
							levels(vegtable@samples[,layers_var])) else
					class(releves[,layers_var]) <- class(vegtable@samples[,
									layers_var])
				if(any(!releves[,layers_var] %in%
								vegtable@layers[[layers_var]][,layers_var]))
					stop("Values of 'layers_var' missing in 'vegtable'")
			}
			# Step 4: Reformat abundance (only for cross tables)
			if(format == 1) {
				if(any(!abundance %in% colnames(vegtable@samples)))
					stop("Some values of 'abundance' are not yet included in 'vegtable'.")
				if(length(abundance) == 2) {
					cover <- stri_split_fixed(releves[,ncol(releves)],
							split_string)
					cover <- lapply(cover, function(x) {
								if(length(x) < 2) x <- c(x, NA)
								return(x)
							})
					cover <- do.call(rbind, cover)
					releves[,ncol(releves)] <- cover[,1]
					releves[,ncol(releves) + 1] <- cover[,2]
					colnames(releves)[ncol(releves) - c(1,0)] <- abundance
				} else colnames(releves)[ncol(releves)] <- abundance
				for(i in abundance) {
					if(is.factor(vegtable@samples[,i]))
						releves[,i] <- factor(paste(releves[,i]),
								levels(vegtable@samples[,i])) else
						class(releves[,i]) <- class(vegtable@samples[,i])
				}
			}
			# Step 5: Format header
			if(!missing(header)) {
				colnames(header)[1] <- "ReleveID"
				if(any(duplicated(header$ReleveID)))
					stop("Duplicated plot names are not allowed.")
				if(!all(releves$ReleveID %in% header$ReleveID))
					stop("Some plots in 'releves' are not included in 'header'.")
				if(!all(header$ReleveID %in% releves$ReleveID))
					stop("Some plots in 'header' are not included in 'releves'.")
				if(!all(colnames(header) %in% colnames(vegtable@header)))
					stop("Some variables in 'header' are not yet included in 'vegtable'.")
				for(i in colnames(header)[-1]) {
					if(is.factor(vegtable@header[,i]))
						header[,i] <- factor(paste(header[,i]),
								levels(vegtable@header[,i])) else
						class(header[,i]) <- class(vegtable@header[,i])
				}
			} else header <- data.frame(ReleveID=unique(releves$ReleveID),
						stringsAsFactors=FALSE)
			# Step 6: Assembly vegtable
			old_ReleveID <- header$ReleveID
			header$ReleveID <- max(vegtable$ReleveID) + 1:nrow(header)
			releves$ReleveID <- header$ReleveID[match(releves$ReleveID,
							old_ReleveID)]
			message(paste0("Imported relev\u00e9s: ", nrow(header),
							" (", min(header$ReleveID), " to ",
							max(header$ReleveID),")"))
			message(paste("Imported header variables:", ncol(header) - 1))
			for(i in colnames(vegtable@header))
				if(!i %in% colnames(header)) header[,i] <- NA
			vegtable@header <- do.call(rbind, list(vegtable@header,
							header[,colnames(vegtable@header)]))
			for(i in colnames(vegtable@samples))
				if(!i %in% colnames(releves)) releves[,i] <- NA
			vegtable@samples <- do.call(rbind, list(vegtable@samples,
							releves[,colnames(vegtable@samples)]))
			message("DONE")
			return(vegtable)
		}
)
