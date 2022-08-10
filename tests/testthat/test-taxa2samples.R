context("inserting taxonomic information to samples")

test_that("taxa2samples is working", {
  Kenya_veg
  expect_false(any(is.na(taxa2samples(Kenya_veg)@samples$TaxonConceptID)))
  expect_true(all(names(Kenya_veg@species@taxonRelations) %in%
    names(taxa2samples(Kenya_veg, add_relations = TRUE)@samples)))
  expect_true(all(names(Kenya_veg@species@taxonTraits) %in%
    names(taxa2samples(Kenya_veg, add_traits = TRUE)@samples)))
  rank_names <- c("species", "genus")
  ranks <- taxa2samples(Kenya_veg,
    add_relations = TRUE,
    include_levels = rank_names
  )@samples$Level
  rank_stats <- summary(ranks[!is.na(ranks)])
  expect_true(all(rank_stats[!names(rank_stats) %in% rank_names] == 0))
  expect_error(taxa2samples(Kenya_veg,
    add_relations = TRUE,
    merge_to = "plantae"
  ))
  expect_error(taxa2samples(Kenya_veg,
    add_relations = TRUE,
    include_levels = c(rank_names, "plant", "animal")
  ))
  expect_true(any(is.na(taxa2samples(Kenya_veg,
    merge_to = "family"
  )@samples$TaxonConceptID)))
})
