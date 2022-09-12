context("converting objects to coverconvert")

test_that("df2coverconvert is working", {
  x <- as(Kenya_veg@coverconvert, "list")$br_bl
  y <- as(Kenya_veg@coverconvert, "list")
  z <- Kenya_veg@coverconvert
  expect_is(df2coverconvert(y), "coverconvert")
  y2 <- y
  names(y2[[2]])[2] <- "bottom_value"
  expect_error(df2coverconvert(y2))
  expect_error(df2coverconvert(x))
  expect_warning(z$cover1 <- z)
})
