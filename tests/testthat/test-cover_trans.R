context("conversion of cover values to percentage")

test_that("cover_trans is working", {
  expect_is(cover_trans(Kenya_veg, to = "percent"), "vegtable")
  expect_true("percent" %in% names(cover_trans(Kenya_veg,
    to = "percent"
  )@samples), "vegtable")
})
