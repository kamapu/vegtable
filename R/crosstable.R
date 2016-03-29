# TODO:   Make cross table from database list
# 
# Author: Miguel Alvarez
################################################################################

setGeneric("crosstable", function(data, species, layers, plots, cover,
				progress="windows", ...) {
			cross <- list()
			if(missing(layers)) {
				cross$layers <- aggregate(as.formula(paste(cover,"~",species)),
						data=data, mean)[,c(species)]
				cross$cover <- matrix(nrow=length(cross$layers),
						ncol=length(unique(paste(data[,plots]))),
						dimnames=list(rownames=1:(length(cross$layers)),
								colnames=unique(paste(data[,plots]))))
				progress <- pmatch(progress, c("windows","text"))
				if(is.na(progress[1])) stop("set valid value for progress bar")
				if(progress[1] == 1) {
					pb <- winProgressBar(min=0, max=dim(data)[1], width=300)
				} else {
					pb <- txtProgressBar(min=0, max=dim(data)[1], width=300)
				}
				for(i in 1:(dim(data)[1])) {
					Sys.sleep(0.1)
					if(progress[1] == 1) {
						setWinProgressBar(pb, i,
								title=paste(round(i/dim(data)[1]*100, 0),
										"% done"))
					} else {
						setTxtProgressBar(pb, i,
								title=paste(round(i/dim(data)[1]*100, 0),
										"% done"))
					}
					cross$cover[cross$layers == data[i,species],
							colnames(cross$cover) == data[i,plots]] <- data[i,
									cover]
				}
				close(pb)
			} else {
				cross$layers <- aggregate(as.formula(paste(cover,"~",layers,"+",
										species)), data=data, mean)[,
						c(species,layers)]
				cross$cover <- matrix(nrow=dim(cross$layers)[1],
						ncol=length(unique(paste(data[,plots]))),
						dimnames=list(rownames=1:(dim(cross$layers)[1]),
								colnames=unique(paste(data[,plots]))))
				progress <- pmatch(progress, c("windows","text"))
				if(is.na(progress[1])) stop("set valid value for progress bar")
				if(progress[1] == 1) {
					pb <- winProgressBar(min=0, max=dim(data)[1], width=300)
				} else {
					pb <- txtProgressBar(min=0, max=dim(data)[1], width=300)
				}
				for(i in 1:(dim(data)[1])) {
					Sys.sleep(0.1)
					if(progress[1] == 1) {
						setWinProgressBar(pb, i,
								title=paste(round(i/dim(data)[1]*100, 0),
										"% done"))
					} else {
						setTxtProgressBar(pb, i,
								title=paste(round(i/dim(data)[1]*100, 0),
										"% done"))
					}
					cross$cover[cross$layers[,species] == data[i,species] &
									cross$layers[,layers] == data[i,layers],
							colnames(cross$cover) == data[i,plots]] <- data[i,
									cover]
				}
				close(pb)
			}
			cross <- data.frame(cross$layers, cross$cover, check.names=FALSE)
			if(missing(layers)) colnames(cross)[1] <- species
			return(cross)
		})

setMethod("crosstable", signature(data="VegTable"), function(data, species,
				layers, plots, cover, progress="windows", ...) {
			Data <- data@samples
			Cross <- crosstable(Data, species, layers, plots, cover, progress,
					...)
			return(Cross)
		})

## crosstable <- function(data, species, layers, plots, cover, progress="windows",
##         ...) {
##     cross <- list()
##     if(missing(layers)) {
##         cross$layers <- aggregate(as.formula(paste(cover,"~",species)),
##                 data=data, mean)[,c(species)]
##         cross$cover <- matrix(nrow=length(cross$layers),
##                 ncol=length(unique(paste(data[,plots]))),
##                 dimnames=list(rownames=1:(length(cross$layers)),
##                         colnames=unique(paste(data[,plots]))))
##         progress <- pmatch(progress, c("windows","text"))
##         if(is.na(progress[1])) stop("set valid value for progress bar")
##         if(progress[1] == 1) {
##             pb <- winProgressBar(min=0, max=dim(data)[1], width=300)
##         } else {
##             pb <- txtProgressBar(min=0, max=dim(data)[1], width=300)
##         }
##         for(i in 1:(dim(data)[1])) {
##             Sys.sleep(0.1)
##             if(progress[1] == 1) {
##                 setWinProgressBar(pb, i,
##                         title=paste(round(i/dim(data)[1]*100, 0),
##                                 "% done"))
##             } else {
##                 setTxtProgressBar(pb, i,
##                         title=paste(round(i/dim(data)[1]*100, 0),
##                                 "% done"))
##             }
##             cross$cover[cross$layers == data[i,species],
##                     colnames(cross$cover) == data[i,plots]] <- data[i,
##                             cover]
##         }
##         close(pb)
##     } else {
##         cross$layers <- aggregate(as.formula(paste(cover,"~",layers,"+",
##                                 species)), data=data, mean)[,
##                 c(species,layers)]
##         cross$cover <- matrix(nrow=dim(cross$layers)[1],
##                 ncol=length(unique(paste(data[,plots]))),
##                 dimnames=list(rownames=1:(dim(cross$layers)[1]),
##                         colnames=unique(paste(data[,plots]))))
##         progress <- pmatch(progress, c("windows","text"))
##         if(is.na(progress[1])) stop("set valid value for progress bar")
##         if(progress[1] == 1) {
##             pb <- winProgressBar(min=0, max=dim(data)[1], width=300)
##         } else {
##             pb <- txtProgressBar(min=0, max=dim(data)[1], width=300)
##         }
##         for(i in 1:(dim(data)[1])) {
##             Sys.sleep(0.1)
##             if(progress[1] == 1) {
##                 setWinProgressBar(pb, i,
##                         title=paste(round(i/dim(data)[1]*100, 0),
##                                 "% done"))
##             } else {
##                 setTxtProgressBar(pb, i,
##                         title=paste(round(i/dim(data)[1]*100, 0),
##                                 "% done"))
##             }
##             cross$cover[cross$layers[,species] == data[i,species] &
##                             cross$layers[,layers] == data[i,layers],
##                     colnames(cross$cover) == data[i,plots]] <- data[i,
##                             cover]
##         }
##         close(pb)
##     }
##     cross <- data.frame(cross$layers, cross$cover, check.names=FALSE)
##     if(missing(layers)) colnames(cross)[1] <- species
##     return(cross)
## }

## setGeneric("crosstable",
##         function(species, layers, plots, cover, data, progress, ...)
##             standardGeneric("crosstable")
## )


