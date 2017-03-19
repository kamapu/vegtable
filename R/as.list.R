# TODO:   Coersion to list
# 
# Author: Miguel Alvarez
################################################################################

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
