context("inserting new relations")

test_that("new_relation is working", {
  veg <- Kenya_veg
  expect_error(new_relation(veg) <- "COVERSCALE")
  expect_is(new_relation(veg, "REMARKS"), "vegtable")
  ## expect_is(new_relation(veg) <- "REMARKS", "vegtable")
  expect_warning(new_relation(veg, c("REMARKS", "REMARKS")))
  veg <- Kenya_veg
  veg$REMARKS <- as.factor(veg$REMARKS)
  expect_is(new_relation(veg, "REMARKS"), "vegtable")
  expect_warning(new_relation(veg, c("REMARKS", "REMARKS")))
  veg <- Kenya_veg
  veg$REMARKS <- as.factor(veg$REMARKS)
  veg <- new_relation(veg, "REMARKS", c(levels(veg$REMARKS), "z"))
  expect_true("REMARKS" %in% names(veg@relations))
  expect_error(new_relation(veg) <- "land_use")
  expect_is(
    new_relation(veg, "land_use",
      levels = c("forest", "grassland", "cropland")
    ), "vegtable"
  )
  ## expect_is(
  ##   new_relation(veg,
  ##     levels = c("forest", "grassland", "cropland")
  ##   ) <- "land_use",
  ##   "vegtable"
  ## )
})

test_that("replacement for new_relation is working", {
  veg <- Kenya_veg
  # New variable as character
  categories <- c("white", "red", "blue", "orange", "green")
  veg$new_var <- sample(categories, nrow(veg@header), replace = TRUE)
  new_relation(veg, levels = categories) <- "new_var"
  expect_true("new_var" %in% names(veg@relations))
  # New variable as factor
  veg$new_var1 <- factor(sample(categories, nrow(veg@header),
    replace = TRUE
  ), levels = categories)
  new_relation(veg) <- "new_var1"
  expect_true("new_var1" %in% names(veg@relations))
})
