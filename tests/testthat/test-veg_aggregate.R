context("aggregating information into data frames")

test_that("veg_aggregate is working", {
  veg <- cover_trans(x = Kenya_veg, to = "cover")
  veg$ALTITUDE[is.na(veg$ALTITUDE)] <- 0
  expect_is(
    veg_aggregate(cover ~ AcceptedName + REFERENCE, veg, mean),
    "data.frame"
  )
  expect_is(
    veg_aggregate(cover ~ TaxonName + REFERENCE, veg, mean),
    "data.frame"
  )
  expect_is(
    veg_aggregate(REFERENCE ~ AcceptedName, veg, length),
    "data.frame"
  )
  expect_is(
    veg_aggregate(cover ~ lf_behn_2018 + ReleveID, veg, mean),
    "data.frame"
  )
  expect_is(
    veg_aggregate(ALTITUDE ~ COMM_TYPE, veg, mean),
    "data.frame"
  )
  expect_error(veg_aggregate(
    cover ~ AcceptedName + TaxonName + ReleveID,
    veg, mean
  ))
})
