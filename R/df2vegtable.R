# TODO:   Convert data frames into vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# Generic function
setGeneric("df2vegtable",
        function(x, species, layer, ...)
            standardGeneric("df2vegtable")
)

# Set method for data frame
setMethod("df2vegtable", signature(x="data.frame"),
        function(x, species, layer, ...) {
            # First creates species list
            taxlist <- new("taxlist")
            taxlist <- add_concept(taxlist, TaxonName=x[,species])
            # Some tests previous to run the function
            if(!missing(layer)) {
                Cover <- x[,-c(species,layer)]
                Layer <- as.factor(x[,layer])
            } else {
                Cover <- x[,-species]
                Layer <- rep(NA, nrow(Cover))
            }
            ReleveID <- list()
            for(i in colnames(Cover)) ReleveID[[i]] <- rep(i, nrow(Cover))
            ReleveID <- do.call(c, ReleveID)
            TaxonUsageID <- rep(taxlist@taxonNames$TaxonUsageID, ncol(Cover))
            Layer <- rep(Layer, ncol(Cover))
            Cover <- do.call(c, Cover)
            return(new("vegtable",
                            samples=data.frame(
                                    ReleveID=as.integer(ReleveID),
                                    Layer=Layer,
                                    TaxonUsageID=TaxonUsageID,
                                    Cover=Cover,
                                    stringsAsFactors=FALSE)[!is.na(Cover) &
                                            Cover > 0,],
                            header=data.frame(
                                    ReleveID=unique(as.integer(ReleveID))),
                            species=taxlist)
            )
        }
)
