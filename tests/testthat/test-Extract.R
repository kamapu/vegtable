context("indexing vegtable objects")

test_that("indexing is working", {
      expect_is(Kenya_veg$REFERENCE, "factor")
      expect_is(Kenya_veg[1:10, 1:5], "vegtable")
    })
