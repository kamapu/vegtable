context("inserting relations into header")

test_that("relation2header is working", {
  expect_is(relation2header(Kenya_veg, "REFERENCE"), "vegtable")
  expect_is(relation2header(Kenya_veg, "REFERENCE", "YEAR"), "vegtable")
  expect_error(relation2header(
    Kenya_veg, "REFERENCE",
    c("the_ref", "the_id")
  ))
  expect_error(relation2header(Kenya_veg, "the_land_use"))
})
