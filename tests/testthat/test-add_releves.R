context("adding releves to vegtable objects")

test_that("add_releves is working", {
  Bussmann2002 <- subset(
    x = Kenya_veg, subset = REFERENCE == 2974,
    slot = "relations", relation = "REFERENCE"
  )
  Bronner1990 <- subset(
    x = Kenya_veg, subset = REFERENCE == 2331,
    slot = "relations", relation = "REFERENCE"
  )
  cross_table <- crosstable(
    formula = b_bbds ~ ReleveID + TaxonName + LAYER,
    data = Bronner1990[1:10, ],
    FUN = function(x) paste0(x, collapse = ".")
  )
  db_list <- cross2db(object = cross_table, layers = TRUE)
  colnames(db_list) <- replace_x(
    x = colnames(db_list),
    old = c("plot", "species", "layers", "cover"),
    new = c("ReleveID", "TaxonName", "LAYER", "b_bbds")
  )
  expect_is(
    add_releves(
      vegtable = Bussmann2002,
      releves = cross_table, abundance = "b_bbds",
      layers = TRUE, layers_var = "LAYER"
    ),
    "vegtable"
  )
  expect_is(
    add_releves(
      vegtable = Bussmann2002, releves = db_list,
      abundance = "b_bbds", layers = TRUE,
      layers_var = "LAYER", format = "databaselist"
    ),
    "vegtable"
  )
  expect_is(
    add_releves(
      vegtable = Bussmann2002, releves = db_list,
      abundance = "b_bbds", layers = TRUE,
      layers_var = "LAYER", format = "databaselist"
    ),
    "vegtable"
  )
  ## expect_is((add_releves(vegtable = Bussmann2002,
  ##                             abundance = "b_bbds", layers = TRUE,
  ##                             layers_var = "LAYER") <- cross_table),
  ##         "vegtable")
  ## expect_is((add_releves(vegtable = Bussmann2002,
  ##                             abundance = "b_bbds", layers = TRUE,
  ##                             layers_var = "LAYER") <- db_list),
  ##         "vegtable")
})
