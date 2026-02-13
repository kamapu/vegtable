# Class vegtable.

Class holding vegetation-plot data sets. Designed to content all
information stored in **Turboveg** databases in just one object.

This class was designed to include information of relevés, header data
and species in just one object. Objects can be created by calls of the
form `new("vegtable", ...)`.

## Slots

- `description`:

  A named character vector containing metadata.

- `samples`:

  A data frame with samples list.

- `header`:

  A data frame with plots data.

- `species`:

  Species list as a
  [taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
  object.

- `layers`:

  A list including strata within samples as data frames.

- `relations`:

  A list including popup lists as data frames.

- `coverconvert`:

  A scale conversion object of class
  [coverconvert](http://kamapu.github.io/vegtable/reference/coverconvert.md).

- `syntax`:

  A list including syntaxonomic lists either as data frames or as
  [taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
  objects.

## See also

[`tv2vegtable()`](http://kamapu.github.io/vegtable/reference/tv2vegtable.md)

## Examples

``` r
showClass("vegtable")
#> Class "vegtable" [package "vegtable"]
#> 
#> Slots:
#>                                                                        
#> Name:   description      samples       layers       header      species
#> Class:    character   data.frame         list   data.frame      taxlist
#>                                              
#> Name:     relations coverconvert       syntax
#> Class:         list coverconvert         list
```
