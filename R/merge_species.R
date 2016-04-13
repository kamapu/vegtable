# TODO:   Method of merge_species for vegtable objects
# 
# Author: Miguel Alvarez
################################################################################

setwd("M:/WorkspaceEclipse/vegtables")

# Method for taxlist -----------------------------------------------------------
setMethod("merge_species", signature(taxlist="vegtable"),
        function(taxlist, ...) {
            if(class(taxlist@species) != "taxlist")
                stop("'taxlist' should be an object of class vegtable")
            taxlist@species <- merge_species(taxlist@species)
            return(taxlist)
        }
)
