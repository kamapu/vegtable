# Aggregating information into a data frame

Compute summarizing tables from
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
objects. This function works in a similar way as
[`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md).

## Usage

``` r
veg_aggregate(object, data, FUN, ...)

# S4 method for class 'formula,vegtable,function'
veg_aggregate(object, data, FUN, use_nas = TRUE, ...)
```

## Arguments

- object:

  A formula indicating the variables used for the summary. As in
  [`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md),
  the keywords `"TaxonName"` and `"AcceptedName"` can be used to
  retrieve taxonomic names, where the second will set the accepted name
  for names considered as synonyms.

- data:

  Either a data frame or an object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- FUN:

  Function used to aggregate values.

- ...:

  Further arguments passed to the function
  [`stats::aggregate()`](https://rdrr.io/r/stats/aggregate.html).

- use_nas:

  Logical value indicating whether NA's should be included in
  categorical variables or not.

## Value

An object of class [data.frame](https://rdrr.io/r/base/data.frame.html).

## See also

[`aggregate()`](https://rdrr.io/r/stats/aggregate.html)

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Transform cover to percentage cover
veg <- cover_trans(x = Kenya_veg, to = "cover")

## Frequency of taxa per publication
atab <- veg_aggregate(object = cover ~ AcceptedName + REFERENCE, data = veg, FUN = length)
head(atab)
#>           AcceptedName REFERENCE cover
#> 1             Abutilon      2331    34
#> 2      Abutilon hirtum      2331    26
#> 3  Abutilon longicuspe      2331     7
#> 4 Abutilon mauritianum      2331    84
#> 5     Abutilon ramosum      2331     3
#> 6               Acacia      2331     1

## Life form proportions per plot
atab <- veg_aggregate(object = cover ~ lf_behn_2018 + ReleveID, data = veg, FUN = sum)
head(atab)
#>         lf_behn_2018 ReleveID cover
#> 1                         358   154
#> 2     climbing_plant      358     1
#> 3 facultative_annual      358     1
#> 4    obligate_annual      358     2
#> 5      reptant_plant      358     1
#> 6      tussock_plant      358     1
```
