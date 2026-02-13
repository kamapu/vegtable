# Count taxa included in vegtable objects

Counting number of taxa within
[taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
objects or character vectors containing taxon names.

This function provides a quick calculation of taxa in
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
objects, considering only records in slot samples. Such records can be
also merged from lower ranks.

For the formula method, units without any requested taxa will not appear
in the output data frame. If no taxa at all is occurring at the
requested level in any unit, an error message will be retrieved.

## Usage

``` r
# S4 method for class 'vegtable,missing'
count_taxa(object, level, include_lower = FALSE, ...)

# S4 method for class 'formula,vegtable'
count_taxa(
  object,
  data,
  include_lower = FALSE,
  suffix = "_count",
  in_header = TRUE,
  ...
)

count_taxa(data, ...) <- value

# S4 method for class 'vegtable,formula'
count_taxa(data, ...) <- value
```

## Arguments

- object:

  An object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  or a formula.

- level:

  Character value indicating the taxonomic rank of counted taxa.

- include_lower:

  Logical value, whether lower taxonomic ranks should be included at the
  requested level.

- ...:

  further arguments passed among methods.

- data:

  An object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- suffix:

  Character value used as suffix on the calculated variable.

- in_header:

  Logical value, whether the result should be included in the slot
  header of the input
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object or not. If the formula term is related to a categorical
  variable at header, the result will be inserted in the respective
  table at slot **relations**.

- value:

  A formula passed to parameter 'object' by the replace method.

## Value

An data frame with the number of taxa from requested level at requested
units for the formula method, or just an integer value.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Different alternatives
count_taxa(Kenya_veg)
#> [1] 1573
head(count_taxa(~ReleveID, Kenya_veg, in_header = FALSE))
#>   ReleveID taxa_count
#> 1      358         58
#> 2      359         50
#> 3      360         50
#> 4      361         64
#> 5      362         68
#> 6      363         67
head(count_taxa(species ~ ReleveID, Kenya_veg, in_header = FALSE))
#>   ReleveID species_count
#> 1      358            55
#> 2      359            48
#> 3      360            46
#> 4      361            60
#> 5      362            63
#> 6      363            63
head(count_taxa(species ~ ReleveID, Kenya_veg, TRUE, in_header = FALSE))
#>   ReleveID species_count
#> 1      358            55
#> 2      359            48
#> 3      360            46
#> 4      361            60
#> 5      362            64
#> 6      363            64
head(count_taxa(family ~ ReleveID, Kenya_veg, TRUE))
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 660.1 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 1 
#>    plots with records: 1 
#>    variables in header: 12 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 3164 
#>    taxon concepts: 2392 
#>    validity: TRUE 
#> 
```
