# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtable)

names(Kenya_veg@samples) <- replace_x(names(Kenya_veg@samples),
    old = "LAYER", new = "layer")
new_layer(Kenya_veg) <- veg_layers


Kenya_veg
summary(Kenya_veg@samples$layer)

data(Kenya_veg)
new_layer(Kenya_veg) <- "layer"
new_layer(Kenya_veg) <- "LAYER"

Kenya_veg
summary(Kenya_veg@samples$LAYER)
