# TODO:   Methods applied to modify the shaker
# 
# Author: Miguel Alvarez
################################################################################

# Function setting species groups ----------------------------------------------
setGeneric("set_group",
        function(shaker, content, group, ...)
            standardGeneric("set_group")
)

# Method for 'taxlist' objects
setMethod("set_group", signature(shaker="shaker", content="taxlist",
                group="character"),
        function(shaker, content, group, group_id, ...) {
            content <- accepted_name(content)
            if(any(!group %in% content$TaxonName))
                warning("Some names in 'group' are not in 'content'")
            if(any(duplicated(content$TaxonName)))
                warning("Some duplicated names in 'content', only one will be retrieved")
            if(missing(group_id))
                group_id <- length(shaker@groups) + 1
            shaker@groups[[group_id]] <- content[charmatch(group,
                            content$TaxonName),"TaxonConceptID"]
            return(shaker)
        }
)

# Method for 'vegtable' objects
setMethod("set_group", signature(shaker="shaker", content="vegtable",
                group="character"),
        function(shaker, content, group, ...) {
            set_group(shaker, content@species, group, ...)
        }
)

# Function setting pseudos -----------------------------------------------------
setGeneric("set_pseudo",
        function(shaker, content, pseudo, ...)
            standardGeneric("set_pseudo")
)

# Method for 'taxlist' objects
setMethod("set_pseudo", signature(shaker="shaker", content="taxlist",
                pseudo="character"),
        function(shaker, content, pseudo, pseudo_id, ...) {
            content <- accepted_name(content)
            if(any(!pseudo %in% content$TaxonName))
                warning("Some names in 'pseudo' are not in 'content'")
            if(any(duplicated(content$TaxonName)))
                warning("Some duplicated names in 'content', only one will be retrieved")
            if(missing(pseudo_id))
                pseudo_id <- length(shaker@pseudos) + 1
            shaker@pseudos[[pseudo_id]] <- content[charmatch(pseudo,
                            content$TaxonName),"TaxonConceptID"]
            return(shaker)
        }
)

# Method for 'vegtable' objects
setMethod("set_pseudo", signature(shaker="shaker", content="vegtable",
                pseudo="character"),
        function(shaker, content, pseudo, ...) {
            set_pseudo(shaker, content@species, pseudo, ...)
        }
)

# TODO: a method for inserting dominance and formulas at once

# TODO: summary method
