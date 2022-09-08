context("calculating traits statistics and proportions")

test_that("trait_stats is working", {
  veg <- cover_trans(Kenya_veg, to = "cover", rule = "middle")
  veg@species@taxonTraits$new_trait <- sample(0:9,
    nrow(veg@species@taxonTraits),
    replace = TRUE
  )
  veg$clust <- sample(letters[1:4], nrow(veg@header), replace = TRUE)
  weighted_mean <- function(x, w, ...) sum(x * w, ...) / sum(w, ...)
  expect_is(trait_stats(new_trait ~ ReleveID, veg,
    FUN = weighted_mean, weight = "cover", suffix = "_wmean",
    na.rm = TRUE
  ), "vegtable")
  expect_is(trait_stats(new_trait ~ REFERENCE, veg,
    FUN = weighted_mean, weight = "cover", suffix = "_wmean",
    na.rm = TRUE
  ), "vegtable")
  expect_is(trait_stats(new_trait ~ ReleveID, veg,
    FUN = weighted_mean, weight = "cover", suffix = "_wmean",
    in_header = FALSE, merge_to = "species"
  ), "data.frame")
  expect_is(trait_stats(new_trait ~ clust, veg,
    FUN = weighted_mean, weight = "cover", suffix = "_wmean",
    na.rm = TRUE
  ), "vegtable")
  # Wrong trait name
  expect_error(trait_stats(new_trait2 ~ ReleveID, veg,
    FUN = weighted_mean, weight = "cover"
  ))
  # Wrong header variable
  expect_error(trait_stats(new_trait ~ releve_id, veg,
    FUN = weighted_mean, weight = "cover"
  ))
  # Wrong or missing weight
  expect_error(trait_stats(new_trait ~ ReleveID, veg,
    FUN = weighted_mean, weight = "the_cover"
  ))
  expect_error(trait_stats(new_trait ~ ReleveID, veg,
    FUN = weighted_mean
  ))
  # Wrong taxonomic ranks
  expect_error(trait_stats(new_trait ~ ReleveID, veg,
    FUN = weighted_mean, weight = "cover", suffix = "_wmean",
    taxon_levels = "plants"
  ))
  expect_error(trait_stats(new_trait ~ ReleveID, veg,
    FUN = weighted_mean, weight = "cover", suffix = "_wmean",
    merge_to = "plants"
  ))
})

test_that("trait_proportion is working", {
  veg <- cover_trans(Kenya_veg, to = "cover", rule = "middle")
  veg@species <- tax2traits(veg@species, get_names = TRUE)
  veg$clust <- sample(letters[1:4], nrow(veg@header), replace = TRUE)
  expect_true("Cyperaceae_prop" %in%
    names(trait_proportion("family", veg,
      trait_level = "Cyperaceae",
      weight = "cover", include_nas = FALSE,
      in_header = TRUE
    )@header))
  expect_is(trait_proportion(family ~ COMM_TYPE, veg,
    weight = "cover",
    in_header = FALSE
  ), "data.frame")
  expect_is(trait_proportion(lf_behn_2018 ~ clust, veg,
    merge_to = "species"
  ), "vegtable")
  expect_error(trait_proportion("family", veg,
    trait_level = "Trees",
    weight = "cover", include_nas = FALSE,
    in_header = TRUE
  ))
  expect_error(trait_proportion(lf_behn_2018 + family ~ ReleveID, veg,
    in_header = TRUE
  ))
})
