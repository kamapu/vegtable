# TODO:   Import cover conversion for vegtable data
# 
# Author: Miguel Alvarez
################################################################################

import_coverconvert <- function(file, as.is=TRUE) {
    file <- read.dbf(file, as.is)
    file <- split(file, file$SCALE_NR)
    for(i in names(file)) {
        short.name <- file[[i]]$SCALE_CODE
        long.name <- file[[i]]$SCALE_NAME
        cover1 <- t(file[[i]][,seq(4, dim(file[[i]])[2], 2)])
        cover2 <- t(file[[i]][,seq(5, dim(file[[i]])[2], 2)])
        file[[i]] <- data.frame(value=cover1[,1][!is.na(cover1[,1])],
                percent=cover2[,1][!is.na(cover1[,1])])
        attr(file[[i]], "short.name") <- short.name
        attr(file[[i]], "long.name") <- long.name
    }
}
