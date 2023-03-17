context("counting taxa")

test_that("counting taxa is working", {
  expect_is(
    count_taxa(object = Kenya_veg, level = "species"),
    "integer"
  )
  expect_is(
    count_taxa(
      object = Kenya_veg, level = "species",
      include_lower = TRUE
    ),
    "integer"
  )
  expect_is(
    count_taxa(object = species ~ ReleveID, data = Kenya_veg),
    "vegtable"
  )
  expect_is(
    count_taxa(
      object = species ~ ReleveID, data = Kenya_veg,
      in_header = FALSE
    ),
    "data.frame"
  )
})

test_that("error messages are accordingly retrieved", {
  expect_error(count_taxa(Kenya_veg, level = "plant"))
  expect_error(count_taxa(species ~ ReleveID + COUNTRY, data = Kenya_veg))
  expect_error(count_taxa(species ~ superpower, data = Kenya_veg))
  expect_error(count_taxa(megaspecies ~ ReleveID, data = Kenya_veg))
})
