# TODO:   Import cover conversion for vegtable data
# 
# Author: Miguel Alvarez
################################################################################

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
        conversion@value[[short_name]] <- cover1
        conversion@conversion[[short_name]] <- c(0, cover2)
    }
    return(conversion)
}
