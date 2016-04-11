# TODO:   Import vegetation dataset into a "VegTable" object
# 
# Author: Miguel Alvarez
################################################################################

import_vegtable <- function(db, tv_home=tv.home(), skip_empty_popups=TRUE) {
    # Empty object
    VEG <- new("vegtable")
    VEG@description <- unlist(c(db, read.dbf(file.path(tv_home, "Data", db,
                            "tvwin.dbf"), as.is=TRUE)[,c("FLORA","DICTIONARY"),
                    drop=FALSE]))
    names(VEG@description) <- c("db.name","sp.list","dictionary")
    # Importing samples
    VEG@samples <- read.dbf(file.path(tv_home, "Data", db, "tvabund.dbf"),
            as.is=TRUE)
    names(VEG@samples) <- TCS.replace(names(VEG@samples))
    # Importing coverconvert in a list format
    if(!is.na(VEG@description["dictionary"])) {
        VEG@coverconvert <- import_coverconvert(file.path(tv_home, "popup",
                        VEG@description["dictionary"], "tvscale.dbf"))
    } else {
        VEG@coverconvert <- import_coverconvert(file.path(tv_home, "popup",
                        "tvscale.dbf"))
    }
    # Importing head data
    VEG@head <- read.dbf(file.path(tv_home, "Data", db, "tvhabita.dbf"),
            as.is=TRUE)
    rownames(VEG@head) <- VEG@head$RELEVE_NR
    # Formating dates and some numeric variables
    VEG@head$DATE <- as.Date(VEG@head$DATE, format="%Y%m%d")
    VEG@head$ALTITUDE <- as.numeric(VEG@head$ALTITUDE)
    VEG@head$INCLINATIO <- as.numeric(VEG@head$INCLINATIO)
    # deleting variables without content
    cat("zero values will be replaced by NAs", "\n")
    cat("variables without values in head will be deleted", "\n")
    for(i in colnames(VEG@head)) {
        if(is.numeric(VEG@head[,i])) VEG@head[,i][VEG@head[,i] == 0] <- NA
    }
    VEG@head <- VEG@head[,!apply(VEG@head, 2, function(x) all(is.na(x)))]
    # Importing popups
    if(is.na(VEG@description["dictionary"])) {
        popups_path <- file.path(tv_home, "popup")
    } else popups_path <- file.path(tv_home, "popup",
                VEG@description["dictionary"])
    Files <- list.files(popups_path, pattern=".dbf", ignore.case=TRUE)
    Files <- Files[!toupper(Files) %in% c("DBASEDIC.DBF","FILES.DBF",
                    "TVSCALE.DBF")]
    popups <- list()
    for(i in Files) {
        popups[[i]] <- read.dbf(file.path(popups_path, i), as.is=TRUE)
    }
    names(popups) <- sub(".dbf", "", tolower(names(popups)))
    # some changes are needed in the standard popups
    colnames(popups$country)[1:2] <- c("COUNTRY","COUNTRY_NAME")
    colnames(popups$syntaxa)[1:2] <- c("SYNTAXON","SYNTAXON_NAME")
    colnames(popups$tvauthor)[1] <- c("AUTHOR")
    # Next cases have to be reformatted
    popups$tvauthor[,1] <- as.integer(popups$tvauthor[,1])
    popups$tvprojct[,1] <- as.integer(popups$tvprojct[,1])
    popups$tvrefenc[,1] <- as.integer(popups$tvrefenc[,1])
    popups$syntaxa[,1] <- as.integer(popups$syntaxa[,1])
    # Deleting empty popups
	if(skip_empty_popups) popups <- popups[sapply(popups, nrow) > 0]
    # Popus to vegtable
    for(i in names(popups)) {
        if(colnames(popups[[i]])[1] %in% colnames(VEG@head))
            popup(VEG, i) <- popups[[i]]
    }
    # Adding tails in remarks
	remarks <- read.dbf(file.path(tv_home, "Data", db, "remarks.dbf"),
			as.is=TRUE)
	releves <- remarks$RELEVE_NR
	remarks <- split(remarks, releves)
	for(i in names(remarks)) {
		if(dim(remarks[[i]])[1] > 1) {
			remarks[[i]][1,"REMARKS"] <- paste0(remarks[[i]][,"REMARKS"],
					collapse="")
			remarks[[i]] <- remarks[[i]][1,]
		}
	}
	remarks <- do.call(rbind, remarks)
	rownames(remarks) <- remarks$RELEVE_NR
	for(i in rownames(remarks)) {
		VEG@head[i,"REMARKS"] <- paste0(VEG@head[i,"REMARKS"],
                remarks[i,"REMARKS"], collapse="")
	}
	# Transformation of cover percentage
	coverscale <- VEG@head$COVERSCALE[match(VEG@samples$RELEVE_NR,
                    VEG@head$RELEVE_NR)]
	samples <- split(VEG@samples, coverscale)
	for(i in names(samples)) {
		if(i == "00") {
			samples[[i]]$COVER_PERC <- with(samples[[i]],
					as.numeric(replace(COVER_CODE, COVER_CODE == "9X", "100")))
		} else {
			samples[[i]]$COVER_PERC <- with(VEG@coverconvert[[i]],
					percent[match(samples[[i]]$COVER_CODE, value)])
		}
	}
	VEG@samples <- unsplit(samples, coverscale)
	# Import species
    .UsageIDs <- list(UsageIDs=VEG@samples$TaxonUsageID)
    attach(.UsageIDs)
    VEG@species <- tvsplist(VEG@description["sp.list"], tv_home)
    VEG@species <- subset(VEG@species, TaxonUsageID %in% UsageIDs)
    detach(.UsageIDs)
    # Logging import
	VEG@log[["import"]] <- c(time=paste(Sys.time()), database=db)
	return(VEG)
}
