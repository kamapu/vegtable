context("testing statistic outputs")

test_that("statistics are printed", {
  # Summary of vegtable objects
  result <- evaluate_promise(print(vegtable_stat(Kenya_veg)), print = TRUE)
  expect_true(grepl("## Metadata", result$output))
})
