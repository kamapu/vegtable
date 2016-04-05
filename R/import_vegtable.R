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
		coverconvert <- read.dbf(file.path(tv_home, "popup",
                        VEG@description["dictionary"], "tvscale.dbf"),
                as.is=TRUE)
	} else {
		coverconvert <- read.dbf(file.path(tv_home, "popup", "tvscale.dbf"),
				as.is=TRUE)
	}
	coverconvert <- split(coverconvert, coverconvert$SCALE_NR)
	for(i in names(coverconvert)) {
		short.name <- coverconvert[[i]]$SCALE_CODE
		long.name <- coverconvert[[i]]$SCALE_NAME
		cover1 <- t(coverconvert[[i]][,seq(4, dim(coverconvert[[i]])[2], 2)])
		cover2 <- t(coverconvert[[i]][,seq(5, dim(coverconvert[[i]])[2], 2)])
		coverconvert[[i]] <- data.frame(value=cover1[,1][!is.na(cover1[,1])],
				percent=cover2[,1][!is.na(cover1[,1])])
		attr(coverconvert[[i]], "short.name") <- short.name
		attr(coverconvert[[i]], "long.name") <- long.name
	}
    VEG@coverconvert <- coverconvert
    # Importing popups
    Files <- c(references="tvrefenc", authors="tvauthor",
			projects="tvprojct", countries="country", syntaxa="syntaxa",
			aspect="exposit")
    for(i in names(Files)) {
		if(!is.na(VEG@description["dictionary"])) {
			VEG@popups[[i]] <- read.dbf(file.path(tv_home, "popup",
                            VEG@description["dictionary"], paste0(Files[i],
                                    ".dbf")), as.is=TRUE)
		} else {
			VEG@popups[[i]] <- read.dbf(file.path(tv_home, "popup",
							paste0(Files[i], ".dbf")), as.is=TRUE)
		}
	}
	# Importing header data
	VEG@head <- read.dbf(file.path(tv_home, "Data", db, "tvhabita.dbf"),
            as.is=TRUE)
	rownames(VEG@head) <- VEG@head$RELEVE_NR
	
    
    # check links to popups
	colnames(VEG@popups$authors)[colnames(VEG@popups$authors) ==
                    "AUTH_CODE"] <- "AUTHOR"
	colnames(VEG@popups$syntaxa) <- c("SYNTAXON","SYNTAX_NAME")
	popvars <- c(references="REFERENCE", authors="AUTHOR", projects="PROJECT",
			syntaxa="SYNTAXON")
	for(i in names(popvars)) {
		VEG@head[,popvars[i]] <- as.integer(VEG@head[,popvars[i]])
		VEG@popups[[i]][,popvars[[i]]] <- as.integer(VEG@popups[[i]][,
                        popvars[[i]]])
		if(dim(VEG@popups[[i]])[1] > 0) {
			if(!all(VEG@head[,popvars[i]] %in% VEG@popups[[i]][,popvars[i]])) {
				warning(paste(c("some values of", i, "are missing in the head",
										"or vice versa."), collapse=" "),
						call.=FALSE)
			}
		}
	}
	colnames(VEG@popups$countries) <- c("COUNTRY","COUNTRY_NAME")
	popvars <- c(countries="COUNTRY", aspect="EXPOSITION")
	for(i in names(popvars)) {
		if(dim(VEG@popups[[i]])[1] > 0) {
			if(!all(VEG@head[,popvars[i]] %in% VEG@popups[[i]][,popvars[i]])) {
				warning(paste(c("some values of", i, "are missing in the head",
										"or vice versa."), collapse=" "),
						call.=FALSE)
			}
		}
	}
	# Deleting empty popups
	if(skip_empty_popups) {
		zeropop <- lapply(VEG@popups, nrow)
		VEG@popups <- VEG@popups[zeropop != 0]
	}
	# Formating dates and some numeric variables
	VEG@head$DATE <- as.Date(VEG@head$DATE, format="%Y%m%d")
	VEG@head$ALTITUDE <- as.numeric(VEG@head$ALTITUDE)
	VEG@head$INCLINATIO <- as.numeric(VEG@head$INCLINATIO)
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
	# deleting variables without content
	warning("zero values will be replaced by NAs", call.=FALSE)
	warning("variables without values in head will be deleted", call.=FALSE)
	for(i in colnames(VEG@head)) {
		if(is.numeric(VEG@head[,i])) VEG@head[,i][VEG@head[,i] == 0] <- NA
	}
	VEG@head <- VEG@head[,!apply(VEG@head, 2, function(x) all(is.na(x)))]
	# Transformation of cover percentage
	coverscale <- VEG@head$COVERSCALE[match(VEG@samples$RELEVE_NR,
                    VEG@head$RELEVE_NR)]
	samples <- split(VEG@samples, coverscale)
	for(i in names(samples)) {
		if(i == "00") {
			samples[[i]]$COVER_PERC <- with(samples[[i]],
					as.numeric(replace(COVER_CODE, COVER_CODE == "9X", "100")))
		} else {
			samples[[i]]$COVER_PERC <- with(coverconvert[[i]],
					percent[match(samples[[i]]$COVER_CODE, value)])
		}
	}
	VEG@samples <- unsplit(samples, coverscale)
	# Import species
    VEG@species <- tvsplist(VEG@description["sp.list"], tv_home)
    VEG@species <- subset(VEG@species, TaxonUsageID %in%
                    unique(VEG@samples$TaxonUsageID))
	# Logging import
	VEG@log[["import"]] <- c(time=paste(Sys.time()), database=db)
	return(VEG)
}
