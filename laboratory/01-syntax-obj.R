# TODO:   Experimenting new syntax slot
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable2)
library(dbaccess)
library("RPostgreSQL")

conn <- connect_db2("veg_databases", user = "miguel")

Veg <- import_sam(conn)

# Adding syntaxonomies
Veg@syntax$sam <- db2taxlist(conn,
    taxon_names = c("tax_commons", "syntax_names"),
    taxon_relations = c("syntax_sam", "taxonRelations"),
    taxon_levels = c("tax_commons", "bb_levels"),
    taxon_views = c("bib_references", "main_table"),
    names2concepts = c("syntax_sam", "names2concepts"))

Veg@syntax$community_type <- Veg@relations$community_type
Veg$syntax_community_type <- Veg$community_type

object <- Veg




object@syntax[["community_type"]] <- subset(object@syntax[["community_type"]],
    community_type %in% object$community_type)

object@syntax[["community_type"]] <- object@syntax[["community_type"]][1:50, ]




i <- names(object@syntax)[1]





taxon_names = c("tax_commons", "syntax_names")
taxon_relations = c("syntax_sam", "taxonRelations")
taxon_levels = c("tax_commons", "bb_levels")
taxon_views = c("bib_references", "main_table")
names2concepts = c("syntax_sam", "names2concepts")
subset_levels = TRUE
as_list = FALSE







DBI::dbDisconnect(conn)

