context("retrieve and write relations")

test_that("veg_relation is working", {
  expect_is(
    veg_relation(Kenya_veg, "REFERENCE", match_header = TRUE),
    "data.frame"
  )
  ## expect_is(veg_relation(Kenya_veg) <- data.frame(
  ##         land_use = c("forest", "grassland", "cropland"),
  ##         description = c("bla", "blabla", "blablabla")), "vegtable")
})
