context("indexing vegtable objects")

test_that("indexing is working", {
  expect_is(Kenya_veg$REFERENCE, "factor")
  expect_is(Kenya_veg[1:10, 1:5], "vegtable")
  expect_is(Kenya_veg[1:10, ], "vegtable")
  expect_is(Kenya_veg[, 1:5], "vegtable")
  expect_is(Kenya_veg@coverconvert$br_bl, "coverconvert")
})

test_that("replacement methods are working", {
  cov1 <- Kenya_veg@coverconvert$br_bl
  cov2 <- Kenya_veg@coverconvert$ordin.
  ## cov1$ordin. <- cov2$ordin.
  ## expect_is(cov1, "coverconvert")
  # TODO: Check for better application in source code
  veg <- Kenya_veg
  veg$new_var <- NA
  expect_true("new_var" %in% names(veg@header))
})
