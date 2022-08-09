# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("kamapu/vegtable", "issue-21")

library(vegtable)

Kenya_veg <- taxa2samples(Kenya_veg)
any(is.na(Kenya_veg@samples$TaxonConceptID))

data(Kenya_veg)
head(taxa2samples(Kenya_veg, add_relations = TRUE)@samples)

data(Kenya_veg)
head(taxa2samples(Kenya_veg, add_traits = TRUE)@samples)

data(Kenya_veg)
head(taxa2samples(Kenya_veg, add_traits = TRUE, add_relations = TRUE)@samples)

data(Kenya_veg)
summary(taxa2samples(Kenya_veg, add_relations = TRUE)@samples$Level)

data(Kenya_veg)
summary(taxa2samples(Kenya_veg, add_relations = TRUE,
        merge_to = "species")@samples$Level)

data(Kenya_veg)
summary(taxa2samples(Kenya_veg, add_relations = TRUE,
        merge_to = "family")@samples$Level)

data(Kenya_veg)
summary(taxa2samples(Kenya_veg, add_relations = TRUE,
        include_levels = c("species", "genus"))@samples$Level)

# Count taxa
library(vegtable)

count_taxa(Kenya_veg)

head(count_taxa(~ ReleveID, Kenya_veg, in_header = FALSE))

head(count_taxa(species ~ ReleveID, Kenya_veg, in_header = FALSE))

head(count_taxa(family ~ ReleveID, Kenya_veg, in_header = FALSE))

head(count_taxa(family ~ ReleveID, Kenya_veg, TRUE, in_header = FALSE))


data(Kenya_veg)
Kenya_veg <- count_taxa(~ ReleveID, Kenya_veg)
Kenya_veg <- count_taxa(family ~ ReleveID, Kenya_veg, TRUE)

with(Kenya_veg@header, plot(taxa_count, family_count))
abline(0, 1, lty = 2, col = "red")

library(vegtable)
## object = ~ ReleveID
object = family ~ ReleveID
data = Kenya_veg
include_lower = FALSE
suffix = "_count"
in_header = TRUE


x = TaxonUsageID ~ ReleveID
data = Kenya_veg
FUN = function(x) length(unique(x))
use_nas = TRUE

Test <- data@header[, names(data@header) %in%
        c("ReleveID", Terms), drop = FALSE]


head(count_taxa(species ~ ReleveID, Kenya_veg))
head(count_taxa(species ~ ReleveID, Kenya_veg, TRUE))
head(count_taxa(family ~ ReleveID, Kenya_veg, TRUE))

object = ~ ReleveID
data = Kenya_veg
include_lower = FALSE
suffix = "_count"
in_header = TRUE


Test <- aggregate(TaxonUsageID ~ ReleveID, Kenya_veg,
    function(x) length(unique(x)))

x = TaxonUsageID ~ ReleveID
data = Kenya_veg
FUN = function(x) length(unique(x))
## merge_to
## include_levels
add_relations = FALSE
add_traits = FALSE

