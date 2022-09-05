context("retrieve and write relations")

test_that("veg_relation is working", {
  expect_is(
    veg_relation(Kenya_veg, "REFERENCE", match_header = TRUE),
    "data.frame"
  )
  expect_is(
    veg_relation(Kenya_veg, "REFERENCE"),
    "data.frame"
  )
})

test_that("replacement for veg_relation is working", {
  veg <- Kenya_veg
  categories <- c("forest", "grassland", "cropland")
  # Insert data frame with categories as character
  veg_relation(veg) <- data.frame(
    land_use = categories,
    description = c("bla", "blabla", "blablabla")
  )
  expect_true("land_use" %in% names(veg@relations))
  # Insert data frame with categories as factor
  veg_relation(veg) <- data.frame(
    land_use2 = as.factor(categories),
    description = c("bla", "blabla", "blablabla")
  )
  expect_true("land_use2" %in% names(veg@relations))
  # Insert data frame with existing header
  # TODO: Next is not working
  ## veg$land_use3 <- sample(categories, nrow(veg@header), replace = TRUE)
  ## veg_relation(veg) <- data.frame(
  ##     land_use3 = categories,
  ##     description = c("bla", "blabla", "blablabla"))
  ## expect_true("land_use3" %in% names(veg@relations))
})
