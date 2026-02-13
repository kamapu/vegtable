# Insert a new variable as relation in vegtable object

Insert a new variable in slot **header** with a respective table at slot
**relations**. The respective variable in header will be set as factor.

Existing categorical variables can also be set as relations. If such
variables are factors, its levels can be preserved (missing argument in
`'levels'`) or reset.

## Usage

``` r
new_relation(object, ...)

# S3 method for class 'vegtable'
new_relation(object, relation, levels, ...)

new_relation(object, levels) <- value

# S4 method for class 'vegtable,character,character'
new_relation(object, levels) <- value

# S4 method for class 'vegtable,missing,character'
new_relation(object) <- value
```

## Arguments

- object:

  A
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object.

- ...:

  Further arguments passed among methods.

- relation, value:

  A character value indicating the name of the new relation. The
  parameter 'value' is used for the replacement method

- levels:

  A character vector with the levels for the inserted factor. This may
  be missing for variables that already exist in slot **header**.

## Value

A
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object with the inserted new relation.

## Examples

``` r
## A brand new variable
new_relation(Kenya_veg, levels = c("forest", "grassland", "cropland")) <- "land_use"

## Set an existing variable as relation
new_relation(Kenya_veg) <- "REMARKS"
```
