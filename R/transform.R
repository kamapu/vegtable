#' @name transform
#' 
#' @title Convert cover scales to percent cover
#' 
#' @description 
#' Convert values of a categorical cover scale to percentage values.
#' 
#' This function requires as input a [coverconvert-class] object which contains
#' the conversion tables.
#' 
#' In the case of [vegtable-class] objects, the conversion is already embedded
#' in the slot `coverconvert`.
#' 
#' Three rules are implemented for transformation, either `top` (values
#' transformed to the top of the range), `middle` (transformation at the
#' midpoint), and `bottom` (conversion at the lowest value of the
#' range). In the later case, transformation ranges starting at 0% of cover
#' can be set to a different value by the argument `zeroto`.
#' 
#' When `replace=FALSE`, existing values of cover in the [vegtable-class]
#' object will be maintained. Since there is not a standard naming of cover
#' values, in the transformation the name of cover variable should be
#' indicated in the argument `to`.
#' 
#' @param x Either a factor or character vector, or a [vegtable-class] object.
#' @param conversion An object of class [vegtable-class].
#' @param from Scale name of values in `x` as character value.
#' @param to Name of the column in slot `samples` for writing converted values.
#' @param replace Logical value indicating whether existing cover values should
#'     be replaced or not.
#' @param rule Rule applied for the conversion (see details).
#' @param zeroto Value used to replace levels with bottom at 0% cover.
#' @param ... Further arguments passed from or to other methods.
#' 
#' @return Either a vector or a [vegtable-class] object.
#' 
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @examples
#' ## Check the available scales
#' summary(Kenya_veg@@coverconvert)
#' 
#' ## Conversion by default 'top' rule
#' Kenya_veg <- transform(Kenya_veg, to="percent")
#' summary(as.factor(Kenya_veg@@samples$percent))
#' 
#' ## Conversion by 'middle' rule
#' Kenya_veg <- transform(Kenya_veg, to="percent", rule="middle", replace=TRUE)
#' summary(as.factor(Kenya_veg@@samples$percent))
#' 
#' ## Conversion by 'bottom' rule
#' Kenya_veg <- transform(Kenya_veg, to="percent", rule="bottom", replace=TRUE)
#' summary(as.factor(Kenya_veg@@samples$percent))
#' 
#' @rdname transform
#' 
#' @exportMethod transform
#' 
setGeneric("transform",
        function(x, conversion, ...)
            standardGeneric("transform")
)

#' @rdname transform
#' 
#' @aliases transform,character,coverconvert-method
setMethod("transform", signature(x="character", conversion="coverconvert"),
        function(x, conversion, from=NULL, rule="top", zeroto=0.1, ...) {
            rule <- grep(rule[1], c("top","bottom","middle"), ignore.case=TRUE)
            if(!rule %in% c(1:3))
                stop("Invalid value for argument 'rule'")
            if(any(!x %in% base::levels(conversion@value[[from]])))
                warning(paste("Some values in 'x' are not valid and will be",
								"converted in NAs"))
            top <- conversion@conversion[[from]][-1][match(x,
							base::levels(conversion@value[[from]]))]
            if(rule == 1)
                return(top)
            else {
                ties <- rev(duplicated(rev(conversion@conversion[[from]])))
                bottom <- conversion@conversion[[from]][-length(
                                conversion@conversion[[from]])]
                for(i in 1:length(bottom)) if(ties[i])
                        bottom[i] <- bottom[i - 1]
                bottom <- bottom[match(x,
								base::levels(conversion@value[[from]]))]
                if(rule == 3) return((bottom + top)/2)
                if(rule == 2) {
                    bottom[bottom == 0] <- zeroto
                    return(bottom)
                }
                
            }
        }
)

#' @rdname transform
#' 
#' @aliases transform,factor,coverconvert-method
setMethod("transform", signature(x="factor", conversion="coverconvert"),
        function(x, conversion, ...) {
            x <- paste(x)
            transform(x, conversion, ...)
        }
)

#' @rdname transform
#' 
#' @aliases transform,numeric,coverconvert-method
setMethod("transform", signature(x="numeric", conversion="coverconvert"),
		function(x, conversion, ...) {
			x <- paste(x)
			transform(x, conversion, ...)
		}
)

#' @rdname transform
#' 
#' @aliases transform,vegtable,missing-method
setMethod("transform", signature(x="vegtable", conversion="missing"),
        function(x, to, replace=FALSE, rule="top",
                zeroto=0.1, ...) {
            if(!to %in% colnames(x@samples)) x@samples[,to] <- NA
            if(replace) Selection <- 1:length(x@samples[,to]) else
                Selection <- which(is.na(x@samples[,to]))
            for(i in names(x@coverconvert)) 
				if(i %in% colnames(x@samples)) {
					Selection2 <- which(!is.na(x@samples[,i]))
					Selection2 <- intersect(Selection, Selection2)
					x@samples[Selection2,to] <- transform(x@samples[Selection2,
									i], x@coverconvert, from=i, rule=rule,
							zeroto=zeroto)
				}
            return(x)
        }
)
