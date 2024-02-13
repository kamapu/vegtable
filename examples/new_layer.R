## Modify name in samples
names(Kenya_veg@samples) <- replace_x(names(Kenya_veg@samples),
    old = "LAYER", new = "layer")

## Add installed data frame to layers
new_layer(Kenya_veg) <- veg_layers

## Take a look in the result
summary(Kenya_veg@samples$layer)
