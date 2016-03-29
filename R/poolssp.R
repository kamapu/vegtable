# TODO:   A function pooling subspecies
# 
# Author: Miguel Alvarez
################################################################################

poolssp <- function(vegtable) {
	if(class(vegtable) != "VegTable") stop(
				"poolssp works only with object class VegTable")
	Bin <- strsplit(vegtable@species$TaxonName, " ", fixed=TRUE)
	Agg <- data.frame(TaxonConceptID=as.integer(rownames(vegtable@species)),
			BinaryName=unlist(lapply(Bin, function(x) paste(x[1:2],
										collapse=" "))),
			Length=unlist(lapply(Bin, length)))
	Agg <- Agg[order(Agg$Length),]
	Agg <- split(Agg, duplicated(Agg$BinaryName))
	Agg[["TRUE"]]$Conv <- with(Agg[["FALSE"]],
			TaxonConceptID[match(Agg[["TRUE"]]$BinaryName, BinaryName)])
	# Replacing TaxonConceptID in samples
	if("TRUE" %in% names(Agg)) {
		for(i in 1:nrow(Agg[["TRUE"]])) {
			vegtable@samples$TaxonConceptID[vegtable@samples$TaxonConceptID ==
							Agg[["TRUE"]][i,"TaxonConceptID"]] <-
					Agg[["TRUE"]][i,"Conv"]
		}
		# Logging change
		vegtable@log$replaced.species <- list()
		vegtable@log$replaced.species[[length(
						vegtable@log$replaced.species) + 1]] <-
				with(Agg[["TRUE"]], paste(TaxonConceptID, "to", Conv))
		attr(vegtable@log$replaced.species, "time")[length(
						vegtable@log$replaced.species)] <- paste(Sys.time())
	}
	# Modifying species list
	vegtable@species <- vegtable@species[vegtable@species$TaxonConceptID %in%
					Agg[["FALSE"]]$TaxonConceptID,]
	vegtable@species$Aggregated <- (vegtable@species$TaxonConceptID %in%
				Agg[["TRUE"]]$Conv)
	# Output
	return(vegtable)
}
