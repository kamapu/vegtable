# Insert new classification of vegetation layers

A new information table for vegetation layers.

## Usage

``` r
new_layer(object, layer, ...)

# S4 method for class 'vegtable,data.frame'
new_layer(object, layer, ...)

new_layer(object, ...) <- value

# S4 method for class 'vegtable,data.frame'
new_layer(object, ...) <- value

# S4 method for class 'vegtable,character'
new_layer(object, levels, ...) <- value
```

## Arguments

- object:

  A
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object.

- layer:

  A data frame including information on vegetation layers.

- ...:

  Further arguments passed among methods.

- value:

  Either a data frame or a character value. In the second case, this
  value indicates the name of the variable at slot samples that will be
  set as layer information.

- levels:

  A character vector used to set the levels of the new layer. This is
  only used in the replacement method using a character value. This
  input is mandatory when the new layer does not exist in slot samples,
  otherwise an error message will be retrieved.

## Value

A
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object with the inserted new relation.

## See also

[`layers2samples()`](http://kamapu.github.io/vegtable/reference/layers2samples.md)

## Examples

``` r
## Modify name in samples
names(Kenya_veg@samples) <- replace_x(names(Kenya_veg@samples),
    old = "LAYER", new = "layer")

## Add installed data frame to layers
new_layer(Kenya_veg) <- veg_layers

## Take a look in the result
summary(Kenya_veg@samples$layer)
#>     0     1     2     3     4     5     6     7     8     9 
#> 46582  9014     0     0 18158     0   553     0   234     0 

## Do it with existing values
data(Kenya_veg)
new_layer(Kenya_veg) <- "LAYER"
#> Error in .local(object, ..., value = value): Missing variable 'LAYER' in slot samples. You need to specify 'levels' for this method.
summary(Kenya_veg@samples$LAYER)
#> Length  Class   Mode 
#>      0   NULL   NULL 
```
