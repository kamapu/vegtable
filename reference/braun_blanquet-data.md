# Conversion of Braun-Blanquet codes to cover percentage

Cover values conversion as
[coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md)
object.

Object of class
[coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md)
contains conversion tables usually from a categorical variable (a cover
scale) to a numerical one (equivalent percentage cover value). Cover
values are stored as range for each level in the scale (minimum and
maximum cover value).

## Usage

``` r
braun_blanquet
```

## Format

An object of class
[`coverconvert`](http://kamapu.github.io/vegtable/reference/coverconvert.md).

## See also

[coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md)
[`cover_trans()`](http://kamapu.github.io/vegtable/reference/cover_trans.md)

## Examples

``` r
names(braun_blanquet)
#> [1] "br_bl"  "b_bbds" "ordin."
summary(braun_blanquet)
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
summary(braun_blanquet$b_bbds)
#> ## Number of cover scales: 1 
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
```
