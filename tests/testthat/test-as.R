context("coercing objects")

test_that("as.list is working", {
  expect_is(as(Kenya_veg, "list"), "list")
  expect_is(as(Kenya_veg@coverconvert, "list"), "list")
})
