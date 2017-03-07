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
setMethod("df2vegtable", signature(x="data.frame", species="numeric",
                layer="numeric"),
        function(x, species, layer, ...) {
            # First creates species list
            taxlist <- new("taxlist")
            taxlist <- add_concept(taxlist, TaxonName=unique(paste(x[,
                                            species])))
            # Some tests previous to run the function
            Cover <- x[,-c(species,layer)]
            for(i in 1:ncol(Cover)) Cover[,i] <- paste(Cover[,i])
            Layer <- as.factor(x[,layer])
            ReleveID <- list()
            for(i in colnames(Cover)) ReleveID[[i]] <- rep(i, nrow(Cover))
            ReleveID <- do.call(c, ReleveID)
            
            TaxonUsageID <- taxlist@taxonNames$TaxonUsageID[match(paste(x[,
                                            species]),
                            taxlist@taxonNames$TaxonName)]
            TaxonUsageID <- rep(TaxonUsageID, ncol(Cover))
            Layer <- rep(Layer, ncol(Cover))
            Cover <- do.call(c, Cover)
            x <- new("vegtable",
                            samples=data.frame(
                                    ReleveID=as.integer(ReleveID),
                                    Layer=Layer,
                                    TaxonUsageID=TaxonUsageID,
                                    Cover=Cover,
                                    stringsAsFactors=FALSE),
                            header=data.frame(
                                    ReleveID=unique(as.integer(ReleveID))),
                            species=taxlist)
            x@samples <-x@samples[!is.na(Cover),]
            x@samples <-x@samples[Cover != "NA",]
            return(x)
        }
)

# If layer missing
setMethod("df2vegtable", signature(x="data.frame", species="numeric",
                layer="missing"),
        function(x, species, ...) {
            x[,ncol(x) + 1] <- NA
            df2vegtable(x, species, layer=ncol(x), ...)
        }
)
