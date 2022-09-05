context("retrieving header")

test_that("header is working", {
  expect_is(header(Kenya_veg), "data.frame")
  veg <- Kenya_veg
  expect_error(header(veg) <- iris)
  df <- data.frame(
    ReleveID = veg$ReleveID[1:10],
    new_var = sample(0:9, 10, TRUE)
  )
  header(veg) <- df
  expect_is(veg, "vegtable")
})
