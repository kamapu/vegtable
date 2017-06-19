# TODO:   Alternative for importing juice tables in R
# 
# Author: Miguel Alvarez
################################################################################

read_juice <- function(file, encoding="LATIN-1", sep=",", ...) {
    file <- readLines(file, encoding=encoding, ...)
    # First prepare the header
    header <- file[(which(file == "Table head:") + 2):length(file)]
    header <- do.call(rbind, strsplit(header, sep))
    colnames(header) <- header[1,]
    header <- as.data.frame(header[-1,], stringsAsFactors=FALSE)
    colnames(header)[1:2] <- c("releve_nr","table_nr")
    # Now the cross table
    file <- file[2:(which(file == "Table head:") - 1)]
    file <- file[nchar(file) != 0]
    file <- do.call(rbind, strsplit(file, sep))
    colnames(file) <- file[1,]
    if(colnames(file)[1] == "") {
        colnames(file)[1] <- "species"
        N <- 2
    }
    if(colnames(file)[2] == "") {
        colnames(file)[2] <- "layer"
        N <- 3
    }
    file <- as.data.frame(file[-1,], stringsAsFactors=FALSE)
    for(i in N:ncol(file)) file[,i] <- as.numeric(file[,i])
    return(list(cross_table=file, header=header))
}
