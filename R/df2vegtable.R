#' @name df2vegtable
#'
#' @title Convert a data frame into a vegtable object.
#' 
#' @description 
#' Conversion of a data frame containing a cross table of abundance or cover of
#' species in single plots.
#' 
#' This function coerces a data frame containing a vegetation cross table into
#' a [vegtable-class] object. The input data frame `x` may include information
#' on the layers or not.
#' 
#' @param x A data frame formatted for a taxlist object.
#' @param species Numeric or integer indicating the position of the column with
#'     species names.
#' @param layer Numeric or integer indicating the position of the column with
#'     layers.
#' @param ... Further arguments passed from or to other methods.
#' 
#' @return A [vegtable-class] object.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples
#' ## Creating data set 'dune_veg'
#' library(vegan)
#' 
#' ## Load data from vegan
#' data(dune)
#' data(dune.env)
#' 
#' ## Conversion to vegtable
#' dune_veg <- data.frame(species=colnames(dune), t(dune),
#'     stringsAsFactors=FALSE, check.names=FALSE)
#' dune_veg <- df2vegtable(dune_veg, species=1)
#' 
#' summary(dune_veg)
#' 
#' ## Adding environmental variables
#' dune.env$ReleveID <- as.integer(rownames(dune.env))
#' header(dune_veg) <- dune.env
#' 
#' summary(dune_veg)
#' 
#' @rdname df2vegtable
#' 
#' @exportMethod df2vegtable
#' 
setGeneric("df2vegtable",
        function(x, species, layer, ...)
            standardGeneric("df2vegtable")
)

#' @rdname df2vegtable
#' 
#' @aliases df2vegtable,data.frame,numeric,numeric-method
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

#' @rdname df2vegtable
#' 
#' @aliases df2vegtable,data.frame,numeric,missing-method
setMethod("df2vegtable", signature(x="data.frame", species="numeric",
                layer="missing"),
        function(x, species, ...) {
            x[,ncol(x) + 1] <- NA
            df2vegtable(x, species, layer=ncol(x), ...)
        }
)
