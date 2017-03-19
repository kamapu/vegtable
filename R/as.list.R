# TODO:   Coersion to list
# 
# Author: Miguel Alvarez
################################################################################

S4_to_list <- function(x) {
    out <- list()
    for(i in slotNames(x)) out[[i]] <- slot(x, i)
    return(out)
}

# Method for taxlist
setMethod("as.list", signature(x="taxlist"),
        function(x, ...) {
            S4_to_list(x)
        }
)

# Method for vegtable
setMethod("as.list", signature(x="vegtable"),
        function(x, ...) {
            S4_to_list(x)
        }
)

# Method for coverconvert
setMethod("as.list", signature(x="coverconvert"),
        function(x, ...) {
            S4_to_list(x)
        }
)
