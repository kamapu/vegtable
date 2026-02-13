# Coerce objects to lists

Coerce vegtable objects to a list with every slot as a component of the
list. This way allows to explore content and solve problems when
validity checks fail.

Coercion is applied for different classes by vegtable.

## Usage

``` r
# S4 method for class 'vegtable'
as.list(x, ...)

# S4 method for class 'coverconvert'
as.list(x, ...)
```

## Arguments

- x:

  An object to be coerced.

- ...:

  further arguments passed from or to other methods.

## Value

An object of class `list`.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## vegtable as list
veg <- as(Kenya_veg, "list")
names(veg)
#> [1] "description"  "samples"      "layers"       "header"       "species"     
#> [6] "relations"    "coverconvert" "syntax"      

## coverconvert as list
as(Kenya_veg@coverconvert, "list")
#> $br_bl
#>   value bottom top
#> 1     r      0   1
#> 2     +      0   1
#> 3     1      1   5
#> 4     2      5  25
#> 5     3     25  50
#> 6     4     50  75
#> 7     5     75 100
#> 
#> $b_bbds
#>   value bottom top
#> 1     r      0   1
#> 2     +      0   1
#> 3     1      1   5
#> 4    2m      1   5
#> 5    2a      5  15
#> 6    2b     15  25
#> 7     3     25  50
#> 8     4     50  75
#> 9     5     75 100
#> 
#> $ordin.
#>   value bottom top
#> 1     1      0   1
#> 2     2      0   1
#> 3     3      1   5
#> 4     4      1   5
#> 5     5      5  15
#> 6     6     15  25
#> 7     7     25  50
#> 8     8     50  75
#> 9     9     75 100
#> 
```
