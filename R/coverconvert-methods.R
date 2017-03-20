# TODO:   Some methods applied to coverconvert class
# 
# Author: Miguel Alvarez
################################################################################


# Dollar method ----------------------------------------------------------------
setMethod("$", signature(x="coverconvert"),
        function(x, name) {
            list(value=x@value[[name]], conversion=x@conversion[[name]])
        }
)

setReplaceMethod("$", signature(x="coverconvert", value="list"),
        function(x, name, value) {
            x@value[[name]] <- value$value
            x@conversion[[name]] <- value$conversion
            return(x) 
        }
)

# Transform to percentage cover ------------------------------------------------

# Generic function
setGeneric("transform",
        function(x, conversion, ...)
            standardGeneric("transform")
)

# Method for character vector
setMethod("transform", signature(x="character", conversion="coverconvert"),
        function(x, conversion, from=NULL, rule="top", zeroto=0.1, ...) {
            rule <- grep(rule[1], c("top","bottom","middle"), ignore.case=TRUE)
            if(!rule %in% c(1:3))
                stop("Invalid value for argument 'rule'")
            if(any(!x %in% levels(conversion@value[[from]])))
                warning("Some values in 'x' are not valid and will be converted in NAs")
            top <- conversion@conversion[[from]][-1][match(x,
                            levels(conversion@value[[from]]))]
            if(rule == 1)
                return(top)
            else {
                ties <- rev(duplicated(rev(conversion@conversion[[from]])))
                bottom <- conversion@conversion[[from]][-length(
                                conversion@conversion[[from]])]
                for(i in 1:length(bottom)) if(ties[i])
                        bottom[i] <- bottom[i - 1]
                bottom <- bottom[match(x, levels(conversion@value[[from]]))]
                if(rule == 3) return((bottom + top)/2)
                if(rule == 2) {
                    bottom[bottom == 0] <- zeroto
                    return(bottom)
                }
                
            }
        }
)

# Method for factor
setMethod("transform", signature(x="factor", conversion="coverconvert"),
        function(x, conversion, ...) {
            x <- paste(x)
            transform(x, conversion, ...)
        }
)

# Method for vegtable and coverconvert
setMethod("transform", signature(x="vegtable", conversion="missing"),
        function(x, to, replace=FALSE, rule="top",
                zeroto=0.1, ...) {
            if(!to %in% colnames(x@samples)) x@samples[,to] <- NA
            if(replace) Selection <- 1:length(x@samples[,to]) else
                Selection <- which(is.na(x@samples[,to]))
            for(i in names(x@coverconvert)) {
                Selection2 <- which(!is.na(x@samples[,i]))
                Selection2 <- intersect(Selection, Selection2)
                x@samples[Selection2,to] <- transform(x@samples[Selection2,i],
                        x@coverconvert, from=i, rule=rule, zeroto=zeroto)
            }
            return(x)
        }
)
