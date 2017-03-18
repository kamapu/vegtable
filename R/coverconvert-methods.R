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

# Method for vectors
setMethod("transform", signature(x="character", conversion="coverconvert"),
        function(x, conversion, from=NULL, rule="top", zeroto=0.1, ...) {
            rule <- grep(rule[1], c("top","bottom","middle"), ignore.case=TRUE)
            if(!rule %in% c(1:3))
                stop("Invalid value for argument 'rule'")
            if(any(!x %in% conversion@value[[from]]))
                warning("Some values in 'x' are not valid and will be converted in NAs")
            top <- conversion@conversion[[from]][-1][match(x,
                            conversion@value[[from]])]
            if(rule == 1)
                return(top)
            else {
                ties <- rev(duplicated(rev(conversion@conversion[[from]])))
                bottom <- conversion@conversion[[from]][-length(
                                conversion@conversion[[from]])]
                for(i in 1:length(bottom)) if(ties[i])
                        bottom[i] <- bottom[i - 1]
                bottom <- bottom[match(x, conversion@value[[from]])]
                if(rule == 3) return((bottom + top)/2)
                if(rule == 2) {
                    bottom[bottom == 0] <- zeroto
                    return(bottom)
                }
                
            }
        }
)


