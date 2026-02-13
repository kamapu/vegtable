# Insert variables from relations into header

Information associated to categories listed in slot **relations** can be
inserted to slot **header** for further statistical comparisons.

## Usage

``` r
relation2header(vegtable, ...)

# S3 method for class 'vegtable'
relation2header(vegtable, relation, vars, ...)
```

## Arguments

- vegtable:

  An
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object.

- ...:

  Further arguments passed among methods

- relation:

  A character value indicating the relation to be used for inserting new
  variables in slot header.

- vars:

  A selection of variables from the relation to be inserted in header.
  This function will check the existence of the variables in the
  respective relation and retrieve an error if none is matching the
  names. If missing in the arguments, all variables of the respective
  relation will be inserted.

## Value

A
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object.

## Author

Miguel Alvarez, <kamapu78@gmail.com>

## Examples

``` r
## Insert publication year of the source into header
veg <- relation2header(Kenya_veg, "REFERENCE", "YEAR")

## Show the frequency of plots per publication year
summary(as.factor(veg$YEAR))
#> 1990 1991 1994 2002 2014 
#>  325  693  610  262   56 
```
