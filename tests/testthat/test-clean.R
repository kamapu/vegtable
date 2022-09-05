context("cleaning invalid objects")

test_that("veg_diversity is working", {
  veg <- Kenya_veg
  veg@header <- veg@header[1:10, ]
  expect_equal(validObject(clean(veg)), TRUE)
})
