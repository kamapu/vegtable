context("Cocktail classifications")

test_that("make_cocktail is working", {
  expect_is(
    make_cocktail(Wetlands, Wetlands_veg, cover = "percen"),
    "vegtable"
  )
  expect_is(make_cocktail(Wetlands, Wetlands_veg,
    cover = "percen",
    which = c("HE1", "HE2"), in_header = FALSE
  ), "data.frame")
})

test_that("building shaker is working", {
  W2 <- new("shaker")
  W2 <- set_pseudo(W2, Wetlands_veg, c(
    "Cyperus latifolius",
    "Cyperus exaltatus"
  ))
  W2 <- set_group(W2, Wetlands_veg,
    group_id = "Cyperus papyrus",
    group = c(
      "Cyperus papyrus",
      "Cyclosorus interruptus",
      "Lepistemon owariense"
    )
  )
  W2 <- set_formula(W2, Wetlands_veg,
    formula_id = "HE1",
    formula = "groups:'Cyperus papyrus' | species:'Cyperus papyrus > 50'"
  )
  expect_is(W2, "shaker")
})
