# TODO:   Experiments with add_releves()
# 
# Author: Miguel Alvarez
################################################################################

# 1. Imput data
Bussmann2002 <- subset(x = Kenya_veg, subset = REFERENCE == 2974,
		slot = "relations", relation = "REFERENCE")

Bronner1990 <- subset(x = Kenya_veg, subset = REFERENCE == 2331,
		slot = "relations", relation = "REFERENCE")

cross_table <- crosstable(formula = b_bbds ~ ReleveID + TaxonName + LAYER,
		data = Bronner1990[1:10, ], FUN = function(x) paste0(x, collapse = "."))

db_list <- cross2db(object = cross_table, layers = TRUE)
colnames(db_list) <- replace_x(x = colnames(db_list),
		old = c("plot", "species", "layers", "cover"),
		new = c("ReleveID", "TaxonName", "LAYER", "b_bbds"))

# 2. Merge data sets
Test <- add_releves(vegtable = Bussmann2002, releves = cross_table,
		abundance = "b_bbds", layers = TRUE, layers_var = "LAYER")

summary(Test)

Test2 <- add_releves(vegtable = Bussmann2002, releves = db_list,
		abundance = "b_bbds", layers = TRUE, layers_var = "LAYER",
		format = "databaselist")

summary(Test2)

# 3. Replacement methods
Test3 <- Bussmann2002
add_releves(vegtable = Test3, abundance = "b_bbds", layers = TRUE,
		layers_var = "LAYER") <- cross_table

Test4 <- Bussmann2002
add_releves(vegtable = Test4, abundance = "b_bbds", layers = TRUE,
		layers_var = "LAYER") <- db_list


# For debugging
vegtable = Bussmann2002
releves = cross_table
abundance = "b_bbds"
usage_ids = FALSE
layers = TRUE
layers_var = "LAYER"
format = "crosstable"
preserve_ids = FALSE
