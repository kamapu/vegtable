context("retrieving used synonyms and used concepts")

test_that("used_synonyms is working", {
  expect_is(used_synonyms(Kenya_veg), "data.frame")
})

test_that("used_concepts is working", {
  expect_is(used_concepts(Kenya_veg), "taxlist")
})
