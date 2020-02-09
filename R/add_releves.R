#' @name add_releves
#' 
#' @title Merge relevés from data frames into vegtable objects
#' 
#' @description 
#' Addition of plot observations into existing data sets may implicate merging
#' data frames with [vegtable-class] objects.
#' 
#' Since this function will only update slots **samples** and **header**,
#' consistency with slots **layers**, **relations** and **species** have to be
#' checked and accordingly updated in advance.
#' 
#' @param vegtable An object of class [vegtable-class].
#' @param releves A data frame including plot observations to be added into
#'     `vegtable`.
#' @param header A data frame (optional) including header information for plots.
#' @param abundance A character value (or vector of length 2) indicating the
#'     names of abundance variable in `vegtable`.
#' @param split_string Character value used to split mixed abundance codes.
#' @param usage_ids Logical value indicating whether species are as taxon usage
#'     ids (integers) or names in `releves`.
#' @param layers Logical value indicating whether layers are included in
#'     `releves` or not.
#' @param layers_var Name of the layer variable in `vegtable`.
#' @param format Character value indicating input format of `releves` (either
#'     `"crosstable"` or `"databaselist"`).
#' @param preserve_ids A logical value, whether IDs in input data set should
#'     used as `ReleveID` or not. Those IDs have to be integers and if one
#'     of those already exists in `vegtable`, an error will be retrieved.
#' @param ... Further arguments passed to function [cross2db()] (i.e.
#'     `na_strings`).
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [cross2db()]
#' 
#' @rdname add_releves
#' 
#' @exportMethod add_releves
#' 
setGeneric("add_releves",
		function(vegtable, releves, ...)
			standardGeneric("add_releves")
)

#' @rdname add_releves
#' 
#' @aliases add_releves,vegtable,data.frame-method
#' 
setMethod("add_releves", signature(vegtable="vegtable", releves="data.frame"),
		function(vegtable, releves, header, abundance, split_string,
				usage_ids=FALSE, layers=FALSE, layers_var, format="crosstable",
				preserve_ids=FALSE, ...) {
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
			if(!usage_ids & format == 2) {
				if(!"TaxonName" %in% colnames(releves))
					stop(paste("Colum 'TaxonName' is mandatory in 'releves'",
									"provided as database list."))
				releves$TaxonUsageID <- match_names(releves$TaxonName,
						vegtable)$TaxonUsageID
				# delete column TaxonName
				releves <- releves[,colnames(releves) != "TaxonName"]
			}
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
					stop(paste("Some values of 'abundance' are not yet",
									"included in 'vegtable'."))
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
					stop(paste("Some plots in 'releves' are not included in",
									"'header'."))
				if(!all(header$ReleveID %in% releves$ReleveID))
					stop(paste("Some plots in 'header' are not included in",
									"'releves'."))
				if(!all(colnames(header) %in% colnames(vegtable@header)))
					stop(paste("Some variables in 'header' are not yet",
									"included in 'vegtable'."))
				for(i in colnames(header)[-1]) {
					if(is.factor(vegtable@header[,i]))
						header[,i] <- factor(paste(header[,i]),
								levels(vegtable@header[,i])) else
						class(header[,i]) <- class(vegtable@header[,i])
				}
			} else header <- data.frame(ReleveID=unique(releves$ReleveID),
						stringsAsFactors=FALSE)
			# Step 6: Assembly vegtable
			if(!preserve_ids) {
				old_ReleveID <- header$ReleveID
				if(nrow(vegtable@header) > 0)
					header$ReleveID <- max(vegtable$ReleveID) + 1:nrow(header) else
					header$ReleveID <- 1:nrow(header)
				releves$ReleveID <- header$ReleveID[match(releves$ReleveID,
								old_ReleveID)]
			} else {
				if(any(header$ReleveID %in% vegtable$ReleveID))
					stop(paste("Some IDs in 'releves' already exist in",
									"'vegtable' and cannot be preserved"))
			}
			message(paste0("Imported relev\u00e9s: ", nrow(header),
							" (", min(header$ReleveID), " to ",
							max(header$ReleveID),")"))
			message(paste("Imported header variables:", ncol(header) - 1))
			for(i in colnames(vegtable@header))
				if(!i %in% colnames(header)) header[,i] <- NA
			if(nrow(vegtable@header) > 0)
				vegtable@header <- do.call(rbind, list(vegtable@header,
								header[,colnames(vegtable@header)])) else
				vegtable@header <- header
			# in empty object add releves directly
			if(nrow(vegtable@samples) > 0) {
				for(i in colnames(vegtable@samples))
					if(!i %in% colnames(releves)) releves[,i] <- NA
				vegtable@samples <- do.call(rbind, list(vegtable@samples,
								releves[,colnames(vegtable@samples)]))
			} else vegtable@samples <- releves
			message("DONE")
			return(vegtable)
		}
)
