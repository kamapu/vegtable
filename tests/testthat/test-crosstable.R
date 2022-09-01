context("Crosstables and database lists")

test_that("crosstable is working", {
  veg <- cover_trans(subset(Kenya_veg, REFERENCE == 2331, slot = "header"),
    to = "cover", rule = "middle"
  )
  expect_is(
    crosstable(cover ~ ReleveID + AcceptedName, veg, mean),
    "data.frame"
  )
  expect_is(
    crosstable(cover ~ ReleveID + TaxonName + AuthorName, veg, mean),
    "data.frame"
  )
  expect_is(
    crosstable(cover ~ ReleveID + AcceptedName, veg, mean,
      as_matrix = TRUE, na_to_zero = TRUE
    ),
    "matrix"
  )
  expect_error(crosstable(the_cover ~ ReleveID + AcceptedName, veg, mean))
})

test_that("cross2db is working", {
  veg <- cover_trans(subset(Kenya_veg, REFERENCE == 2331, slot = "header"),
    to = "cover", rule = "middle"
  )
  cross_veg <- crosstable(cover ~ ReleveID + AcceptedName, veg, mean)
  expect_is(cross2db(cross_veg), "data.frame")
})
