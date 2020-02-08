#' @name write_juice
#' 
#' @title Exporting tables for Juice
#' 
#' @description 
#' This function produce txt files as inport formats for **Juice**
#' (\url{http://www.sci.muni.cz/botany/juice/}).
#' 
#' This function produces two output files to be imported into a **Juice**
#' file: A vegetation table produced by [crosstable()] and a header table.
#' Both tables share the file name plus a suffix (`table` for the
#' vegetation table and `header` for the header).
#' 
#' For the import in **Juice**, you may start with the table following in
#' the menu `File -> Import -> Table -> from Spreadsheet File (e.g. EXCEL
#' Table)` and then follow the wizard. You further import the header table
#' following in the menu `File -> Import -> Header Data -> From Comma
#' Delimited File`.
#' Notice that the vegetation is a semi-colon delimited file, while the header
#' is a comma delimited file.
#' 
#' For a properly import, you may strictly follow the export steps in **Juice**:
#' 1) In menu `File -> Export -> Table -> to Spreadsheet Format File` and
#' 2) check the option `Export covers in %`.
#' 
#' In the `header` (see **Value**), the first column (`juice_nr`)
#' corresponds to the plot number assigned by **Juice** at import, while
#' the column `db_nr` is the number originally assigned to the plot (e.g.
#' **Turboveg** ID).
#' 
#' @param data An object of class [vegtable-class].
#' @param file Character value indicating the name of output files (without
#'     file extension).
#' @param db_name Name for data set displayed in inport wizard.
#' @param formula A formula passed to [crosstable()].
#' @param FUN Funtion passed to [crosstable()].
#' @param header Variables of header to be exported.
#' @param coords Names of coordinate variables in header of `data`.
#' @param ... Further arguments. While `write_juice()` passes them to the
#'     function [crosstable()], `read_juice()` passes those arguments to
#'     [readLines()].
#' 
#' @return 
#' For `read_juice()`, a list with two elements: A data frame of species by
#' plot (`cross_table`), and a data frame with header data (`header`).
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples
#' ## Only first 20 observations
#' Kenya_veg <- Kenya_veg[1:20,]
#' \dontrun{
#' write_juice(Kenya_veg, "SWEA", FUN=mean)
#' }
#' 
#' @rdname write_juice
#' 
#' @exportMethod write_juice
#' 
setGeneric("write_juice", function(data, file, formula, ...)
            standardGeneric("write_juice"))

#' @rdname write_juice
#' 
#' @aliases write_juice,vegtable,character,formula-method
#' 
setMethod("write_juice", signature(data="vegtable", file="character",
                formula="formula"),
        function(data, file, formula, FUN, db_name="Plot Observations",
				header, coords, ...) {
            # some attributes
            nr.plots <- nrow(data@header)
            # header
            header.in <- header
            if(attr(terms(formula), "term.labels")[1] != "ReleveID")
                stop("'ReleveID' is mandatory as first term of formula")
            if(!missing(coords)) {
				if(length(coords) != 2)
					stop("Argument 'coords' have to be of length 2.")
                header <- c(header, coords)
                header.in <- c(header.in, c("deg_lon","deg_lat"))
            }
            header <- c("ReleveID", header)
            if(!all(header %in% colnames(data@header)))
                stop("some requested headers are not included in 'data'")
            header <- data@header[,header]
            rownames(header) <- NULL
            # write header
            write.table(t(c("Table number", "Releve number", header.in)),
                    paste(file, "header.txt", sep="_"), quote=FALSE,
                    row.names=FALSE, col.names=FALSE, sep=",")
            write.table(header, paste(file, "header.txt", sep="_"), quote=FALSE,
                    col.names=FALSE, sep=",", na="", append=TRUE)
            # table
            data <- crosstable(formula, data, FUN, ...)
            
            colnames(data)[1:(length(attr(terms(formula),
                                                "term.labels")) - 1)] <- ""
            # write table
            write.table(rbind(db_name, paste("Number of releves:", nr.plots),
                            ""), paste(file, "table.txt", sep="_"), quote=FALSE,
                    row.names=FALSE, col.names=FALSE)
            suppressWarnings(write.table(data, paste(file, "table.txt",
                                    sep="_"), quote=FALSE, row.names=FALSE,
                            na="", sep=";", append=TRUE))
        }
)

#' @rdname write_juice
#' @aliases read_juice
#' 
#' @param encoding Argument passed to \code{\link{readLines}}.
#' @param sep Separator used to split rows into columns.
#' @param na Character used as not available values.
#' 
#' @examples
#' ## Installed 'Juice' version of 'Wetlands_veg'
#' Veg <- file.path(path.package("vegtable"), "juice", "Wetlands_juice.txt")
#' Veg <- read_juice(Veg)
#' 
#' summary(Veg)
#' 
#' @export 
#' 
read_juice <- function(file, encoding="LATIN-1", sep=";", na="", ...) {
	file <- readLines(file, encoding=encoding, ...)
	# First prepare the header
	if("Table head:" %in% file) {
		header <- file[(which(file == "Table head:") + 2):length(file)]
		header <- strsplit(header, sep)
		# Trick for ending NAs
		N <- sapply(header, length)
		if(any(N < max(N))) for(i in 1:length(N)) {
				if(N[i] < max(N)) header[[i]] <- c(header[[i]],
							rep(NA, max(N) - N[i]))
			}
		header <- do.call(rbind, header)
		colnames(header) <- header[1,]
		header <- as.data.frame(header[-1,], stringsAsFactors=FALSE)
		colnames(header)[1:2] <- c("juice_nr","db_nr")
		file <- file[2:(which(file == "Table head:") - 1)]
	} else {
		header <- NULL
		file <- file[-1]
	}
	# Now the cross table
	file <- file[nchar(file) != 0]
	file <- do.call(rbind, strsplit(file, sep))
	colnames(file) <- file[1,]
	N <- integer()
	if(colnames(file)[1] == "") {
		colnames(file)[1] <- "species"
		N <- 2
	}
	if(colnames(file)[2] == "") {
		colnames(file)[2] <- "layer"
		N <- 3
	}
	file <- as.data.frame(file[-1,], stringsAsFactors=FALSE)
	for(i in N:ncol(file)) {
		file[file[,i] == na,i] <- NA
		file[,i] <- as.numeric(file[,i])
	}
	return(list(cross_table=file, header=header))
}

