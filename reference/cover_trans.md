# Convert cover scales to percent cover

Convert values of a categorical cover scale to percentage values.

This function requires as input a
[coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md)
object which contains the conversion tables.

## Usage

``` r
# S4 method for class 'character,coverconvert'
cover_trans(x, conversion, from = NULL, rule = "top", zeroto = 0.1, ...)

# S4 method for class 'factor,coverconvert'
cover_trans(x, conversion, ...)

# S4 method for class 'numeric,coverconvert'
cover_trans(x, conversion, ...)

# S4 method for class 'vegtable,missing'
cover_trans(x, to, replace = FALSE, rule = "top", zeroto = 0.1, ...)
```

## Arguments

- x:

  Either a factor or character vector, or a
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object.

- conversion:

  An object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- from:

  Scale name of values in `x` as character value.

- rule:

  A character value indicating the rule applied for cover
  transformation. Three rules are implemented for transformation, either
  `top` (values transformed to the top of the range), `middle`
  (transformation at the midpoint), and `bottom` (conversion at the
  lowest value of the range). In the later case, if the bottom is zero
  cover, a fictive bottom value can be set by `'zeroto'`

- zeroto:

  Value set for transformation of classes with bottom at 0% cover.

- ...:

  Further arguments passed from or to other methods.

- to:

  Name of the column in slot `samples` for writing converted values.

- replace:

  Logical value indicating whether existing cover values should be
  replaced by the new computed values or not.

## Value

Either a vector or a
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Check the available scales
summary(Kenya_veg@coverconvert)
#> ## Number of cover scales: 3 
#> 
#> * scale 'br_bl': 
#>   Levels    Range
#> 1      r    0 - 1
#> 2      +    0 - 1
#> 3      1    1 - 5
#> 4      2   5 - 25
#> 5      3  25 - 50
#> 6      4  50 - 75
#> 7      5 75 - 100
#> 
#> * scale 'b_bbds': 
#>   Levels    Range
#> 1      r    0 - 1
#> 2      +    0 - 1
#> 3      1    1 - 5
#> 4     2m    1 - 5
#> 5     2a   5 - 15
#> 6     2b  15 - 25
#> 7      3  25 - 50
#> 8      4  50 - 75
#> 9      5 75 - 100
#> 
#> * scale 'ordin.': 
#>   Levels    Range
#> 1      1    0 - 1
#> 2      2    0 - 1
#> 3      3    1 - 5
#> 4      4    1 - 5
#> 5      5   5 - 15
#> 6      6  15 - 25
#> 7      7  25 - 50
#> 8      8  50 - 75
#> 9      9 75 - 100
#> 

## Conversion by default 'top' rule
Kenya_veg <- cover_trans(Kenya_veg, to = "percent")
summary(as.factor(Kenya_veg@samples$percent))
#>     1     5    15    25    50    75   100 
#> 55943  9269  1158  4846  2004   841   480 

## Conversion by 'middle' rule
Kenya_veg <- cover_trans(Kenya_veg, to = "percent", rule = "middle", replace = TRUE)
summary(as.factor(Kenya_veg@samples$percent))
#>   0.5     3    10    15    20  37.5  62.5  87.5 
#> 55943  9269  1158  4364   482  2004   841   480 

## Conversion by 'bottom' rule
Kenya_veg <- cover_trans(Kenya_veg, to = "percent", rule = "bottom", replace = TRUE)
summary(as.factor(Kenya_veg@samples$percent))
#>   0.1     1     5    15    25    50    75 
#> 55943  9269  5522   482  2004   841   480 
```
