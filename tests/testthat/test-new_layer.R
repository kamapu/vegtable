context("inserting new layers")

test_that("layers are inserted", {
  veg <- Kenya_veg
  names(veg@samples) <- replace_x(names(veg@samples),
    old = "LAYER", new = "layer"
  )
  veg_layers2 <- veg_layers
  veg_layers2$layer <- as.character(veg_layers2$layer)
  expect_is(
    {
      new_layer(veg) <- veg_layers2
      veg
    },
    "vegtable"
  )
  veg <- Kenya_veg
  expect_is(
    {
      new_layer(veg) <- veg_layers
      veg
    },
    "vegtable"
  )
  veg <- Kenya_veg
  expect_is(
    {
      new_layer(veg) <- "LAYER"
      veg
    },
    "vegtable"
  )
})

test_that("error messages at 'new_layer()' are working", {
  veg <- Kenya_veg
  expect_error(new_layer(veg) <- "german_layers")
  expect_error({
    new_layer(veg) <- "LAYER"
    new_layer(veg) <- "LAYER"
  })
  veg <- Kenya_veg
  names(veg@samples) <- replace_x(names(veg@samples),
    old = "LAYER", new = "layer"
  )
  veg_layers2 <- do.call(rbind, list(veg_layers, veg_layers))
  expect_error(new_layer(veg) <- veg_layers2)
})
