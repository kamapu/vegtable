context("retrieving names")

test_that("veg_diversity is working", {
      expect_is(names(Kenya_veg), "list")
      expect_is(names(Kenya_veg@coverconvert), "character")
    })
