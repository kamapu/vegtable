# TODO:   Convert cross table to database list
# 
# Author: Miguel Alvarez
################################################################################

cross2db <- function(object, layers=FALSE, na_strings) {
	species <- object[,1]
	if(layers) {
		LAY <- object[,2]
		Cover <- object[,-1:-2]
		object <- lapply(split(1:ncol(Cover), 1:ncol(Cover)),
				function(x, cov, spec, lay) {
					releve <- data.frame(plot=colnames(cov)[x], species=spec,
							layers=lay, cover=cov[,x], stringsAsFactors=FALSE)
					return(releve)
				}, cov=Cover, spec=species, lay=LAY)
	} else {
		Cover <- object[,-1,drop=FALSE]
		object <- lapply(split(1:ncol(Cover), 1:ncol(Cover)),
				function(x, cov, spec) {
					releve <- data.frame(plot=colnames(cov)[x], species=spec,
							cover=cov[,x], stringsAsFactors=FALSE)
					return(releve)
				}, cov=Cover, spec=species)
	}
	object <- do.call(rbind, object)
	if(!missing(na_strings))
		object$cover[paste(object$cover) %in% na_strings] <- NA
	return(object[!is.na(object$cover),])
}
