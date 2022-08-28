context("calculating diversity indices")

test_that("veg_diversity is working", {
  veg <- cover_trans(x = Kenya_veg, to = "cover")
  expect_true("dominance" %in% names(veg_diversity(
    veg, "cover",
    dominance
  )@header))
  expect_true("simpson" %in% names(veg_diversity(
    veg, "cover",
    simpson
  )@header))
  expect_true("richness" %in% names(veg_diversity(
    veg, "cover",
    richness
  )@header))
  expect_error(veg_diversity(veg, "cover_percentage"))
  expect_is(veg_diversity(veg, "cover", in_header = FALSE), "data.frame")
})
