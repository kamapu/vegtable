# TODO:   subset functions for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

# subset method for vegtable objects
setMethod("subset", signature(x="vegtable"),
        function(x, subset, slot="header", ...) {
            ## slot <- grep(slot[1], slotNames(x), ignore.case=TRUE)
            slot <- grep(slot[1], c("samples","header"), ignore.case=TRUE)
            if(length(slot) == 0)
                stop("Invalid value for argument 'slot'")
            slot <- slotNames(x)[slot]
            subset <- substitute(subset)
            subset <- eval(subset, slot(x, slot), parent.frame())
            if(slot == "samples") x@samples <- x@samples[subset,]
            if(slot == "header") x@header <- x@header[subset,]
            return(clean(x))
        }
)
