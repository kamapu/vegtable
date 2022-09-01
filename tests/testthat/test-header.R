context("retrieving header")

test_that("header is working", {
      expect_is(header(Kenya_veg), "data.frame")
    })
