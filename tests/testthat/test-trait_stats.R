context("calculating traits statistics")

test_that("trait_stats is working", {
  veg <- cover_trans(Kenya_veg, to = "cover", rule = "middle")
  veg@species@taxonTraits$new_trait <- sample(0:9,
    nrow(veg@species@taxonTraits),
    replace = TRUE
  )
  expect_true("new_trait_stats" %in%
    names(trait_stats("new_trait", veg, mean)@header))
})

test_that("trait_proportion is working", {
  veg <- cover_trans(Kenya_veg, to = "cover", rule = "middle")
  veg@species <- tax2traits(veg@species, get_names = TRUE)
  expect_true("Cyperaceae_prop" %in%
    names(trait_proportion("family", veg,
      trait_level = "Cyperaceae",
      weight = "cover", include_nas = FALSE,
      in_header = TRUE
    )@header))
})
