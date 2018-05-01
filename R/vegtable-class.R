# TODO:   Set S4 object class "VegTable"
# 
# Author: Miguel Alvarez
################################################################################

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
            }
            # Mandatory links
            if(!all(object@samples$ReleveID %in% object@header$ReleveID))
                return("Some releve IDs from slot 'samples' are missing in slot 'header'")
            if(!all(object@header$ReleveID %in% object@samples$ReleveID))
                return("Some releve IDs from slot 'header' are missing in slot 'samples'")
            if(any(!object@samples[!is.na(!object@samples$TaxonUsageID),
									"TaxonUsageID"] %in%
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
