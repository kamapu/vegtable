# TODO:   Alternative for importing juice tables in R
# 
# Author: Miguel Alvarez
################################################################################

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
