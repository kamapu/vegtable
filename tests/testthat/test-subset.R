context("inserting taxonomic information to samples")

test_that("subset is working", {
  expect_error(subset(Kenya_veg, as.integer(YEAR) >= 2000,
    slot = "animals"
  ))
  expect_is(subset(Kenya_veg, grepl("Cyperus", TaxonName),
    slot = "taxonNames"
  ), "vegtable")
  expect_is(subset(Kenya_veg, TaxonConceptID > 54000,
    slot = "taxonRelations"
  ), "vegtable")
  expect_is(subset(Kenya_veg, TaxonConceptID > 54000,
    slot = "taxonTraits"
  ), "vegtable")
  expect_is(subset(Kenya_veg, br_bl == "4", slot = "samples"), "vegtable")
  expect_is(subset(Kenya_veg, TaxonConceptID > 54000,
    slot = "taxonTraits"
  ), "vegtable")
  expect_is(subset(Kenya_veg, REFERENCE == "3012",
    slot = "relations", relation = "REFERENCE"
  ), "vegtable")
})
