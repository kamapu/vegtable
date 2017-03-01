# TODO:   Set S4 object class "VegTable"
# 
# Author: Miguel Alvarez
################################################################################

setClass("vegtable", slots=c(
				description="character",
				samples="data.frame",
				header="data.frame",
				species="taxlist",
				relations="list",
				coverconvert="ANY"),
		prototype=list(
                description=character(),
                samples=data.frame(
                        ReleveID=integer(),
                        TaxonUsageID=integer()
                ),
				header=data.frame(
                        ReleveID=integer()
                ),
                species=new("taxlist"),
                relations=list(),
				coverconvert=NULL),
		validity=function(object) {
            # Mandatory names
            if(any(!c("ReleveID","TaxonUsageID") %in% colnames(object@samples)))
                return("Columns 'ReleveID' and 'TaxonUsageID' are mandatory in slot 'samples'")
            if(!"ReleveID" %in% colnames(object@header))
                return("Column 'ReleveID' is mandatory in slot 'header'")
            for(i in names(object@relations)) {
                if(!i %in% colnames(object@header))
                    return(paste0("Relation '", i, "' not included in slot 'header'"))
                if(!i %in% colnames(object@relations[[i]]))
                    return(paste0("Column '", i, "' is mandatory in relation '", i, "'"))
            }
            # Mandatory links
            if(any(!unique(object@samples$ReleveID) %in% object@header$ReleveID))
                return("Some releve IDs from slot 'samples' are missing in slot 'header'")
            if(any(!object@samples$TaxonUsageID %in% object@species@taxonNames$TaxonUsageID))
                return("Some taxon names are missing in slot 'species'")
            # Other consistency tests
            if(any(duplicated(object@header$ReleveID)))
                return("Duplicated releve IDs are not allowed in slot 'header'")
        }
)
