# TODO:   Method of merge_taxa for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

setMethod("merge_taxa", signature(taxlist="vegtable"),
        function(taxlist, ...) {
            if(class(taxlist@species) != "taxlist")
                stop("'taxlist' should be an object of class vegtable")
            taxlist@species <- merge_taxa(taxlist@species)
            return(taxlist)
        }
)
