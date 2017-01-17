# TODO:   A method to write
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("write.juice", function(data, file, formula, ...)
            standardGeneric("write.juice"))

# Method for data frames
setMethod("write.juice", signature(data="vegtable", file="character",
                formula="formula"),
        function(data, file, formula=COVER_PERC ~ RELEVE_NR + AcceptedName +
                        LAYER, head=c("COUNTRY","REFERENCE"),
                coords=c("LONGITUDE","LATITUDE"), FUN, ...) {
            # some attributes
            db.name <- data@description["db.name"]
            nr.plots <- nrow(data@head)
            # head
            head.in <- head
            if(attr(terms(formula), "term.labels")[1] != "RELEVE_NR")
                stop("'RELEVE_NR' is mandatory as first term of formula")
            if(length(coords) == 2) {
                head <- c(head, coords)
                head.in <- c(head.in, c("deg_lon","deg_lat"))
            }
            head <- c("RELEVE_NR", head)
            if(!all(head %in% colnames(data@head)))
                stop("some requested heads are not included in 'data'")
            head <- data@head[,head]
            rownames(head) <- NULL
            # write head
            write.table(t(c("Table number", "Releve number", head.in)),
                    paste(file, "head.txt", sep="_"), quote=FALSE,
                    row.names=FALSE, col.names=FALSE, sep=",")
            write.table(head, paste(file, "head.txt", sep="_"), quote=FALSE,
                    col.names=FALSE, sep=",", na="", append=TRUE)
            # table
            data <- crosstable(formula, data, FUN, ...)
            colnames(data)[1:(length(attr(terms(formula),
                                                "term.labels")) - 1)] <- ""
            # write table
            write.table(rbind(db.name, paste("Number of releves:", nr.plots),
                            ""), paste(file, "table.txt", sep="_"), quote=FALSE,
                    row.names=FALSE, col.names=FALSE)
            suppressWarnings(write.table(data, paste(file, "table.txt",
                                    sep="_"), quote=FALSE, row.names=FALSE,
                            na="", sep=";", append=TRUE))
        }
)
