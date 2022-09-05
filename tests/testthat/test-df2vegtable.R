context("calculating diversity indices")

test_that("veg_diversity is working", {
  library(vegan)
  data(dune)
  data(dune.env)
  dune_veg <- data.frame(
    species = colnames(dune), t(dune),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  expect_is(df2vegtable(dune_veg, species = 1), "vegtable")
})
