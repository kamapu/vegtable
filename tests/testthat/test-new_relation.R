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
  ## expect_is(veg <- new_relation(veg, "REMARKS", c(levels(veg$REMARKS), "z")),
  ##     "vegtable")
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
