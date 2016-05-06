<<<<<<< HEAD
<<<<<<< HEAD
# TODO:   Method of merge_species for vegtable objects
=======
# TODO:   Method of merge_taxa for vegtable objects
>>>>>>> refs/heads/miguel
=======
# TODO:   Method of merge_taxa for vegtable objects
>>>>>>> refs/remotes/origin/miguel
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
