context("retrieving names")

test_that("names is working", {
  expect_is(names(Kenya_veg), "list")
  expect_is(dimnames(Kenya_veg), "list")
  expect_is(names(Kenya_veg@coverconvert), "character")
})

test_that("replacement for names is working", {
  cov <- Kenya_veg@coverconvert
  names(cov)[1] <- "Braun-Blanquet"
  expect_equal(names(cov)[1], "Braun-Blanquet")
})
