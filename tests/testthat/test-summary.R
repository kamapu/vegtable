context("testing summary outputs")

test_that("Summaries are produced", {
  # Summary of vegtable objects
  result <- evaluate_promise(print(Kenya_veg), print = TRUE)
  expect_true(grepl("## Metadata", result$output))
  result <- evaluate_promise(show(Kenya_veg), print = TRUE)
  expect_true(grepl("## Metadata", result$output))
  # Summary of coverconvert objects
  result <- evaluate_promise(print(Kenya_veg@coverconvert), print = TRUE)
  expect_true(grepl("## Number of cover scales", result$output))
  result <- evaluate_promise(show(Kenya_veg@coverconvert), print = TRUE)
  expect_true(grepl("## Number of cover scales", result$output))
  # Summary of shaker objects
  result <- evaluate_promise(print(Wetlands), print = TRUE)
  expect_true(grepl("Number of pseudo-species", result$output))
  result <- evaluate_promise(show(Wetlands), print = TRUE)
  expect_true(grepl("Number of pseudo-species", result$output))
  # Summary shaker with companion
  result <- evaluate_promise(summary(Wetlands, Wetlands_veg), print = TRUE)
  expect_true(grepl("## Species groups", result$output))
})


