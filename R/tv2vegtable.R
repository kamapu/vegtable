#' @name tv2vegtable
#' 
#' @title Import of vegetation data from Turboveg databases
#' 
#' @description 
#' Import function for **Turboveg** databases into an object of class
#' [vegtable-class].
#' Most of the contents of **Turboveg** databases are included in DBF files and
#' therefore imported by the function [foreign::read.dbf()].
#' The automatic setting of database path will be done by the function
#' [vegdata::tv.home()] but it can be customised by the argument `tv_home`.
#' 
#' The species list will be imported by using the function
#' [taxlist::tv2taxlist()] and therefore formatted as a [taxlist-class] object.
#' Similarly, conversion tables will be handled as [coverconvert-class] objects.
#' 
#' Empty columns in the header will be deleted in the imported object.
#' 
#' The function `tv2coverconvert()` reads the content of cover conversion
#' tables stored in **Turboveg** and attempts to reformat them in a more
#' comprehensive structure.
#' 
#' This function is used by `tv2vegtable()` to import the respective
#' conversion table from **Turboveg** databases.
#' Note that conversion tables in **Turboveg** have only stored the middle
#' point for each cover class in a scale, thus it will be recommended to
#' rebuild the `coverconvert` slot or use [braun_blanquet].
#' 
#' @param db Name of **Turboveg** data base as character value.
#' @param tv_home **Turboveg** installation path as character value.
#' @param skip_empty_relations Logical value indicating whether empty relations
#'     may be excluded from imported database or not.
#' @param skip_scale Character value indicating scales to be excluded in slot
#'     `coverconvert`.
#' @param clean Logical value indicating whether output object should be
#'     cleaned or not.
#' 
#' @return
#' A [vegtable-class] object in the case of `tv2vegtable()`.
#' A [coverconvert-class] object in the case of `tv2coverconvert()`.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @seealso [taxlist::tv2taxlist()] [foreign::read.dbf()] [vegdata::tv.home()]
#' 
#' @examples
#' ## Installed 'Turboveg' version of 'Fujiwara et al. (2014)'
#' TV_Home <- file.path(path.package("vegtable"), "tv_data")
#' Veg <- tv2vegtable("Fujiwara_2014", TV_Home)
#' summary(Veg)
#' 
#' @rdname tv2vegtable
#' 
#' @export 
#' 
tv2vegtable <- function(db, tv_home=tv.home(), skip_empty_relations=TRUE,
		skip_scale, clean=TRUE) {
    # Import meta data ---------------------------------------------------------
    description <- unlist(c(db, read.dbf(file.path(tv_home, "Data", db,
                                    "tvwin.dbf"), as.is=TRUE)[,c("FLORA",
                                    "DICTIONARY"), drop=FALSE]))
    names(description) <- c("db_name","sp_list","dictionary")
    # Importing samples --------------------------------------------------------
    samples <- read.dbf(file.path(tv_home, "Data", db, "tvabund.dbf"),
            as.is=TRUE)
    colnames(samples)[colnames(samples) == "RELEVE_NR"] <- "ReleveID"
    colnames(samples)[colnames(samples) == "SPECIES_NR"] <- "TaxonUsageID"
    colnames(samples)[grepl("ORIG_NAME", colnames(samples))] <- "ORIG_NAME"
    samples$COVER_CODE[samples$COVER_CODE == "9X"] <- "100"
    samples$COVER_CODE[samples$COVER_CODE == "99"] <- "100"
    # Importing header data ----------------------------------------------------
    header <- read.dbf(file.path(tv_home, "Data", db, "tvhabita.dbf"),
            as.is=TRUE)
    colnames(header)[colnames(header) == "RELEVE_NR"] <- "ReleveID"
    # Formating dates and some numeric variables
    header$DATE <- as.Date(header$DATE, format="%Y%m%d")
    header$ALTITUDE <- as.numeric(header$ALTITUDE)
    header$INCLINATIO <- as.numeric(header$INCLINATIO)
    # replacing zero values with NAs
    cat("zero values will be replaced by NAs", "\n")
    for(i in colnames(header)) {
        if(is.numeric(header[,i])) header[,i][header[,i] == 0] <- NA
    }
    remarks <- read.dbf(file.path(tv_home, "Data", db, "remarks.dbf"),
            as.is=TRUE)
    remarks <- split(remarks$REMARKS, remarks$RELEVE_NR)
    for(i in as.integer(names(remarks))) {
        header[header$ReleveID == i, "REMARKS"] <- paste(header[
                        header$ReleveID == i, "REMARKS"],
                paste(remarks[[paste(i)]], collapse=" "), collapse=" ")
    }
    # Importing coverconvert ---------------------------------------------------
    if(!is.na(description["dictionary"])) {
        cover_home <- file.path(tv_home, "popup", description["dictionary"],
                "tvscale.dbf")
    } else cover_home <- file.path(tv_home, "popup", "tvscale.dbf")
    coverconvert <- tv2coverconvert(cover_home)
	if(!missing(skip_scale)) {
		coverconvert@value <- coverconvert@value[
				!names(coverconvert@value) %in% skip_scale]
		coverconvert@conversion <- coverconvert@conversion[
				!names(coverconvert@conversion) %in% skip_scale]
	}
	cover_match <- read.dbf(cover_home, as.is=TRUE)[,c("SCALE_NR","SCALE_NAME",
                    "SCALE_CODE")]
    cover_match$SCALE_CODE <- tolower(sub("/", "_", cover_match$SCALE_CODE,
                    fixed=TRUE))
    cover_code <- samples[,c("ReleveID","COVER_CODE")]
    cover_code$entry <- as.numeric(row.names(samples))
    cover_code <- split(cover_code, header$COVERSCALE[match(samples$ReleveID,
                            header$ReleveID)])
    for(i in names(cover_code)) {
        samples[,cover_match[cover_match$SCALE_NR == i,"SCALE_CODE"]] <-
                cover_code[[i]][match(as.numeric(rownames(samples)),
                                cover_code[[i]]$entry),"COVER_CODE"]
    }
    # Get percentage to numeric
    cover_code <- cover_match[cover_match$SCALE_NR == "00","SCALE_CODE"]
    if(cover_code %in% colnames(samples))
        samples[,cover_code] <- as.numeric(samples[,cover_code])
    for(i in slotNames(coverconvert)) slot(coverconvert, i) <-
                slot(coverconvert, i)[names(slot(coverconvert, i)) !=
                                cover_code]
    # Factorize
    cover_code <- names(coverconvert)[names(coverconvert) != cover_code]
    for(i in cover_code) {
        if(i %in% colnames(samples)) samples[,i] <- factor(samples[,i],
                    levels=base::levels(coverconvert@value[[i]]))
    }
    # Importing relations ------------------------------------------------------
    if(is.na(description["dictionary"])) {
        relations_path <- file.path(tv_home, "popup")
    } else relations_path <- file.path(tv_home, "popup",
                description["dictionary"])
    Files <- list.files(relations_path, pattern=".dbf", ignore.case=TRUE)
    Files <- Files[!toupper(Files) %in% c("dbasedic.dbf","files.dbf",
                    "tvscale.dbf")]
    relations <- list()
    for(i in Files) {
        relations[[tolower(i)]] <- read.dbf(file.path(relations_path, i),
                as.is=TRUE)
    }
    # some changes are needed in the standard relations
    colnames(relations$country.dbf)[1:2] <- c("COUNTRY","COUNTRY_NAME")
    colnames(relations$syntaxa.dbf)[1:2] <- c("SYNTAXON","SYNTAXON_NAME")
    colnames(relations$tvauthor.dbf)[1] <- c("AUTHOR")
    # Next cases have to be reformatted
    relations$tvauthor.dbf[,1] <- as.integer(relations$tvauthor.dbf[,1])
    relations$tvprojct.dbf[,1] <- as.integer(relations$tvprojct.dbf[,1])
    relations$tvrefenc.dbf[,1] <- as.integer(relations$tvrefenc.dbf[,1])
    relations$syntaxa.dbf[,1] <- as.integer(relations$syntaxa.dbf[,1])
    # Deleting empty relations
	if(skip_empty_relations) relations <- relations[sapply(relations, nrow) > 0]
    # Rename relations as first column
    names(relations) <- sapply(sapply(relations, colnames), "[", 1)
    # Insert details of cover scales in relations
    relations[["COVERSCALE"]] <- cover_match
    colnames(relations[["COVERSCALE"]])[1] <- "COVERSCALE"
    # Final object
    VEG <- new("vegtable")
    VEG@description <- description
    VEG@samples <- samples
    VEG@header <- header
    VEG@species <- tv2taxlist(description["sp_list"], tv_home)
    VEG@coverconvert <- coverconvert
    for(i in names(relations)) {
        if(colnames(relations[[i]])[1] %in% colnames(VEG@header))
            veg_relation(VEG, i) <- relations[[i]]
    }
    if(clean) VEG <- clean(VEG)
    return(VEG)
}

#' @rdname tv2vegtable
#' @aliases tv2coverconvert
#' 
#' @param file A connection to a DBF file containing conversion table in
#'     **Turboveg**.
#' @param as.is A logical value passed to [read.dbf()].
#' 
#' @examples
#' ## Installed 'Turboveg' version of "Fujiwara et al. (2014)"
#' TV_Home <- file.path(path.package("vegtable"), "tv_data", "popup", "Swea")
#' Table <- tv2coverconvert(file.path(TV_Home, "tvscale.dbf"))
#' 
#' ## First scale have to be deleted from conversion table
#' Table@@value <- Table@@value[-1]
#' Table@@conversion <- Table@@conversion[-1]
#' summary(Table)
#' 
#' ## Compare the 'Turboveg' version with a vegtable version
#' data(braun_blanquet)
#' summary(Table$br_bl)
#' summary(braun_blanquet$br_bl)
#' 
#' @export 
#' 
tv2coverconvert <- function(file, as.is=TRUE) {
	file <- read.dbf(file, as.is)
	file <- split(file, file$SCALE_NR)
	conversion <- new("coverconvert")
	for(i in names(file)) {
		short_name <- tolower(sub("/", "_", file[[i]]$SCALE_CODE, fixed=TRUE))
		cover1 <- t(file[[i]][,seq(4, dim(file[[i]])[2], 2)])[,1]
		cover2 <- t(file[[i]][,seq(5, dim(file[[i]])[2], 2)])[,1]
		cover2 <- cover2[!is.na(cover1)]
		cover1 <- cover1[!is.na(cover1)]
		cover1 <- cover1[order(cover2)]
		cover2 <- cover2[order(cover2)]
		names(cover1) <- names(cover2) <- NULL
		conversion@value[[short_name]] <- factor(cover1, levels=cover1)
		conversion@conversion[[short_name]] <- c(0, cover2)
	}
	return(conversion)
}
