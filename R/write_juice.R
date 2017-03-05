# TODO:   A method to write
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("write_juice", function(data, file, formula, ...)
            standardGeneric("write_juice"))

# Method for data frames
setMethod("write_juice", signature(data="vegtable", file="character",
                formula="formula"),
        function(data, file, formula=CoverPercent ~ ReleveID + AcceptedName +
                        LAYER, header=c("COUNTRY","REFERENCE"),
                coords=c("LONGITUDE","LATITUDE"), FUN, ...) {
            # some attributes
            db_name <- data@description["db_name"]
            nr.plots <- nrow(data@header)
            # header
            header.in <- header
            if(attr(terms(formula), "term.labels")[1] != "ReleveID")
                stop("'ReleveID' is mandatory as first term of formula")
            if(length(coords) == 2) {
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
