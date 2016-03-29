# TODO:   Import vegetation dataset into a "VegTable" object
# 
# Author: Miguel Alvarez
################################################################################

importVegTable <- function(db, tv_home, skip_empty_popups=TRUE) {
	if(missing(tv_home)) tv_home <- tv.home()
	# Reading attributes of database
	ATTR <- as.list(read.dbf(file.path(tv_home, "Data", db,"tvwin.dbf"),
					as.is=TRUE))
	# Importing samples
	samples <- read.dbf(file.path(tv_home, "Data", db, "tvabund.dbf"),
			as.is=TRUE)
	names(samples) <- TCS.replace(names(samples))
	# Importing coverconvert in a list format
	if(!is.na(ATTR$DICTIONARY)) {
		coverconvert <- read.dbf(file.path(tv_home, "popup", ATTR$DICTIONARY,
						"tvscale.dbf"), as.is=TRUE)
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
	# Importing popups
	Files <- c(references="tvrefenc", authors="tvauthor",
			projects="tvprojct", countries="country", syntaxa="syntaxa",
			aspect="exposit")
	popups <- list()
	for(i in names(Files)) {
		if(!is.na(ATTR$DICTIONARY)) {
			popups[[i]] <- read.dbf(file.path(tv_home, "popup", ATTR$DICTIONARY,
							paste0(Files[i], ".dbf")), as.is=TRUE)
		} else {
			popups[[i]] <- read.dbf(file.path(tv_home, "popup",
							paste0(Files[i], ".dbf")), as.is=TRUE)
		}
	}
	# Importing header data
	HEAD <- read.dbf(file.path(tv_home, "Data", db, "tvhabita.dbf"), as.is=TRUE)
	rownames(HEAD) <- HEAD$RELEVE_NR
	# check links to popups
	colnames(popups$authors)[colnames(popups$authors) ==
					"AUTH_CODE"] <- "AUTHOR"
	colnames(popups$syntaxa) <- c("SYNTAXON","SYNTAX_NAME")
	popvars <- c(references="REFERENCE", authors="AUTHOR", projects="PROJECT",
			syntaxa="SYNTAXON")
	for(i in names(popvars)) {
		HEAD[,popvars[i]] <- as.integer(HEAD[,popvars[i]])
		popups[[i]][,popvars[[i]]] <- as.integer(popups[[i]][,popvars[[i]]])
		if(dim(popups[[i]])[1] > 0) {
			if(!all(HEAD[,popvars[i]] %in% popups[[i]][,popvars[i]])) {
				warning(paste(c("some values of", i, "are missing in the head",
										"or vice versa."), collapse=" "),
						call.=FALSE)
			}
		}
	}
	colnames(popups$countries) <- c("COUNTRY","COUNTRY_NAME")
	popvars <- c(countries="COUNTRY", aspect="EXPOSITION")
	for(i in names(popvars)) {
		if(dim(popups[[i]])[1] > 0) {
			if(!all(HEAD[,popvars[i]] %in% popups[[i]][,popvars[i]])) {
				warning(paste(c("some values of", i, "are missing in the head",
										"or vice versa."), collapse=" "),
						call.=FALSE)
			}
		}
	}
	# Deleting empty popups
	if(skip_empty_popups) {
		zeropop <- lapply(popups, nrow)
		popups <- popups[zeropop != 0]
	}
	# Formating dates and some numeric variables
	HEAD$DATE <- as.Date(HEAD$DATE, format="%Y%m%d")
	HEAD$ALTITUDE <- as.numeric(HEAD$ALTITUDE)
	HEAD$INCLINATIO <- as.numeric(HEAD$INCLINATIO)
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
		HEAD[i,"REMARKS"] <- paste0(HEAD[i,"REMARKS"], remarks[i,"REMARKS"],
				collapse="")
	}
	# deleting variables without content
	warning("zero values will be replaced by NAs", call.=FALSE)
	warning("variables without values in head will be deleted", call.=FALSE)
	for(i in colnames(HEAD)) {
		if(is.numeric(HEAD[,i])) HEAD[,i][HEAD[,i] == 0] <- NA
	}
	HEAD <- HEAD[,!apply(HEAD, 2, function(x) all(is.na(x)))]
	# Transformation of cover percentage
	coverscale <- HEAD$COVERSCALE[match(samples$RELEVE_NR, HEAD$RELEVE_NR)]
	samples <- split(samples, coverscale)
	for(i in names(samples)) {
		if(i == "00") {
			samples[[i]]$COVER_PERC <- with(samples[[i]],
					as.numeric(replace(COVER_CODE, COVER_CODE == "9X", "100")))
		} else {
			samples[[i]]$COVER_PERC <- with(coverconvert[[i]],
					percent[match(samples[[i]]$COVER_CODE, value)])
		}
	}
	samples <- unsplit(samples, coverscale)
	# Import species
	species <- tax("all", ATTR$FLORA, syn=TRUE)
	samples$LETTERCODE <- species$LETTERCODE[match(samples$TaxonUsageID,
					species$TaxonUsageID)]
	samples$TaxonConceptID <- species$TaxonConceptID[match(samples$TaxonUsageID,
					species$TaxonUsageID)]
	species <- subset(species, species$SYNONYM == FALSE)
	species <- subset(species, species$LETTERCODE %in% samples$LETTERCODE)
	rownames(species) <- paste(species$TaxonConceptID)
	# Creating a new VegTable object and writing values
	VEG <- new("VegTable")
	VEG@description <- list(db.name=db, sp.list=ATTR$FLORA,
			dictionary=ATTR$DICTIONARY)
	VEG@samples <- samples; rm("samples")
	VEG@head <- HEAD; rm("HEAD")
	VEG@species <- species; rm("species")
	VEG@popups <- popups; rm("popups")
	VEG@coverconvert <- coverconvert; rm("coverconvert")
	VEG@log[["import"]] <- c(time=paste(Sys.time()), database=db)
	return(VEG)
}
