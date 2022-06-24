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
