################### #
## Taxonomy      ####
################### #

aggs <- parsing.result$species.aggs
AGG <- stack(aggs)
#AGG <- stack(lapply(aggs, function(x) trim(sapply(x, substr, 1, nchar(x)-1, USE.NAMES = FALSE))), stringsAsFactors = FALSE)
# this command has resulted in an error because the trailing "0" had
# already been removed by a change in the trim.trailing function
AGG$ind <- as.character(AGG$ind)
AGG <- AGG[AGG$values != AGG$ind,]
AGG <- AGG[AGG$values != '',]
index1 <- match(obs$TaxonName, AGG$values)
obs$TaxonName[!is.na(index1)] <- AGG$ind[index1[!is.na(index1)]]
