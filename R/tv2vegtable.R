# TODO:   Import vegetation dataset into a "VegTable" object
# 
# Author: Miguel Alvarez
################################################################################

tv2vegtable <- function(db, tv_home=tv.home(), skip_empty_relations=TRUE) {
    # Empty object
    description <- unlist(c(db, read.dbf(file.path(tv_home, "Data", db,
                                    "tvwin.dbf"), as.is=TRUE)[,c("FLORA",
                                    "DICTIONARY"), drop=FALSE]))
    names(description) <- c("db_name","sp_list","dictionary")
    # Importing samples
    samples <- read.dbf(file.path(tv_home, "Data", db, "tvabund.dbf"),
            as.is=TRUE)
    colnames(samples)[colnames(samples) == "RELEVE_NR"] <- "ReleveID"
    colnames(samples)[colnames(samples) == "SPECIES_NR"] <- "TaxonUsageID"
    # Importing coverconvert in a list format
    if(!is.na(description["dictionary"])) {
        coverconvert <- import_coverconvert(file.path(tv_home, "popup",
                        description["dictionary"], "tvscale.dbf"))
    } else {
        coverconvert <- import_coverconvert(file.path(tv_home, "popup",
                        "tvscale.dbf"))
    }
    # Importing header data
    header <- read.dbf(file.path(tv_home, "Data", db, "tvhabita.dbf"),
            as.is=TRUE)
    colnames(header)[colnames(header) == "RELEVE_NR"] <- "ReleveID"
    # Formating dates and some numeric variables
    header$DATE <- as.Date(header$DATE, format="%Y%m%d")
    header$ALTITUDE <- as.numeric(header$ALTITUDE)
    header$INCLINATIO <- as.numeric(header$INCLINATIO)
    # deleting variables without content
    cat("zero values will be replaced by NAs", "\n")
    cat("variables without values in header will be deleted", "\n")
    for(i in colnames(header)) {
        if(is.numeric(header[,i])) header[,i][header[,i] == 0] <- NA
    }
    header <- header[,!apply(header, 2, function(x) all(is.na(x)))]
    # Importing relations
    if(is.na(description["dictionary"])) {
        relations_path <- file.path(tv_home, "popup")
    } else relations_path <- file.path(tv_home, "popup",
                description["dictionary"])
    Files <- list.files(relations_path, pattern=".dbf", ignore.case=TRUE)
    Files <- Files[!toupper(Files) %in% c("DBASEDIC.DBF","FILES.DBF",
                    "TVSCALE.DBF")]
    relations <- list()
    for(i in Files) {
        relations[[i]] <- read.dbf(file.path(relations_path, i), as.is=TRUE)
    }
    names(relations) <- sub(".dbf", "", tolower(names(relations)))
    # some changes are needed in the standard relations
    colnames(relations$country)[1:2] <- c("COUNTRY","COUNTRY_NAME")
    colnames(relations$syntaxa)[1:2] <- c("SYNTAXON","SYNTAXON_NAME")
    colnames(relations$tvauthor)[1] <- c("AUTHOR")
    # Next cases have to be reformatted
    relations$tvauthor[,1] <- as.integer(relations$tvauthor[,1])
    relations$tvprojct[,1] <- as.integer(relations$tvprojct[,1])
    relations$tvrefenc[,1] <- as.integer(relations$tvrefenc[,1])
    relations$syntaxa[,1] <- as.integer(relations$syntaxa[,1])
    # Deleting empty relations
	if(skip_empty_relations) relations <- relations[sapply(relations, nrow) > 0]
    
    # Adding tails in remarks
	remarks <- read.dbf(file.path(tv_home, "Data", db, "remarks.dbf"),
			as.is=TRUE)
    remarks <- split(remarks$REMARKS, remarks$RELEVE_NR)
    for(i in as.integer(names(remarks))) {
        header[header$ReleveID == i, "REMARKS"] <- paste(header[header$ReleveID == i,
                        "REMARKS"], paste(remarks[[paste(i)]], collapse=" "),
                collapse=" ")
    }
	# Transformation of cover percentage
	coverscale <- header$COVERSCALE[match(samples$ReleveID, header$ReleveID)]
	cover_trans <- split(samples[,c("ReleveID","COVER_CODE")], coverscale)
	for(i in names(cover_trans)) {
		if(i == "00") {
			cover_trans[[i]]$CoverPercent <- with(cover_trans[[i]],
					as.numeric(replace(COVER_CODE, COVER_CODE == "9X", "100")))
		} else {
			cover_trans[[i]]$CoverPercent <- with(coverconvert[[i]],
					percent[match(cover_trans[[i]]$COVER_CODE, value)])
		}
	}
	cover_trans <- unsplit(cover_trans, coverscale)
    samples$CoverPercent <- cover_trans[match(samples$ReleveID,
                    cover_trans$ReleveID),"CoverPercent"]
    # Final object
    VEG <- new("vegtable",
            description=description,
            samples=samples,
            header=header,
            species=tv2taxlist(description["sp_list"], tv_home),
            coverconvert=coverconvert)
    # Popus to vegtable
    for(i in names(relations)) {
        if(colnames(relations[[i]])[1] %in% colnames(VEG@header))
            veg_relation(VEG, i) <- relations[[i]]
    }
    return(VEG)
}
