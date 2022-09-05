context("coercing objects")

test_that("as.list is working", {
  expect_is(as(Kenya_veg, "list"), "list")
  expect_is(as(Kenya_veg@coverconvert, "list"), "list")
})

test_that("as<- is working", {
  veg <- Kenya_veg
  as(veg) <- "list"
  expect_is(veg, "list")
  cover_t <- Kenya_veg@coverconvert
  as(cover_t) <- "list"
  expect_is(cover_t, "list")
})
