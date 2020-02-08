#' @name vegtable-class
#' @aliases vegtable
#' 
#' @title Class vegtable.
#' 
#' @description 
#' Class holding vegetation-plot data sets.
#' Designed to content all information stored in **Turboveg** databases in
#' just one object.
#' 
#' This class was designed to include information of relevés, header data and
#' species in just one object. Objects can be created by calls of the form
#' `new("vegtable", ...)`.
#' 
#' @slot description A named character vector containing metadata.
#' @slot samples A data frame with samples list.
#' @slot header A data frame with plots data.
#' @slot species Species list as a [taxlist-class] object.
#' @slot layers A list including strata within samples as data frames.
#' @slot relations A list including popup lists as data frames.
#' @slot coverconvert A scale conversion object of class [coverconvert-class].
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [tv2vegtable()]
#' 
#' @examples
#' showClass("vegtable")
#' 
#' @exportClass vegtable
#' 
setClass("vegtable",
        slots=c(
				description="character",
				samples="data.frame",
				layers="list",
				header="data.frame",
				species="taxlist",
				relations="list",
				coverconvert="coverconvert"),
		prototype=list(
                description=character(),
                samples=data.frame(
                        ReleveID=integer(),
						TaxonUsageID=integer()
                ),
				layers=list(),
				header=data.frame(
                        ReleveID=integer()
                ),
                species=new("taxlist"),
                relations=list(),
				coverconvert=new("coverconvert")),
		validity=function(object) {
            # Mandatory names
            if(any(!c("ReleveID","TaxonUsageID") %in% colnames(object@samples)))
                return("Columns 'ReleveID' and 'TaxonUsageID' are mandatory in slot 'samples'")
			for(i in names(object@layers)) {
				if(!i %in% colnames(object@samples))
					return(paste0("Layers of '", i, "' not included in slot 'samples'"))
				if(!i %in% colnames(object@layers[[i]]))
					return(paste0("Column '", i, "' is mandatory in layer table '", i, "'"))
			}
			if(!"ReleveID" %in% colnames(object@header))
                return("Column 'ReleveID' is mandatory in slot 'header'")
            for(i in names(object@relations)) {
                if(!i %in% colnames(object@header))
                    return(paste0("Relation '", i, "' not included in slot 'header'"))
                if(!i %in% colnames(object@relations[[i]]))
                    return(paste0("Column '", i, "' is mandatory in relation '", i, "'"))
				if(any(!object@header[,i] %in% object@relations[[i]][,i] & !is.na(object@header[,i])))
					return(paste0("Some values of '", i,
									"' in header do not macht the values in slot relations."))
            }
            # Mandatory links
            if(!all(object@samples$ReleveID %in% object@header$ReleveID))
                return("Some releve IDs from slot 'samples' are missing in slot 'header'")
            if(!all(object@header$ReleveID %in% object@samples$ReleveID))
                return("Some releve IDs from slot 'header' are missing in slot 'samples'")
            if(any(is.na(object@samples$TaxonUsageID)))
				return("NAs are not allowed in 'TaxonUsageID' at slot 'samples'.")
			if(any(!object@samples$TaxonUsageID %in%
							object@species@taxonNames$TaxonUsageID))
                return("Some taxon names are missing in slot 'species'")
            # Other consistency tests
            if(any(duplicated(object@header$ReleveID)))
                return("Duplicated releve IDs are not allowed in slot 'header'")
			# Tests for layers
			for(i in names(object@layers)) {
				if(any(!object@samples[!is.na(object@samples[,i]), i] %in% object@layers[[i]][,i]))
					return(paste0("Some values of'", i,
									"' in slot 'samples' are missing in slot 'layers'"))
			}
        }
)
