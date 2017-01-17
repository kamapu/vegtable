# TODO:   Method of merge_taxa for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

setMethod("merge_taxa", signature(object="vegtable"),
        function(object, ...) {
            if(class(object@species) != "taxlist")
                stop("'object' should be an object of class vegtable")
            object@species <- merge_taxa(object@species)
            return(object)
        }
)
