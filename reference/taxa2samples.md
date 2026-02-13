# Insert taxon information into samples

For statistical purposes it may be necessary to insert information on
recorded taxa into the slot samples, which contain the records of taxa
in sampling plots. This can be also done selectivelly for specific
taxonomic ranks and lower ranks can be aggregated to their parental
ones.

If column **TaxonConceptID** is already existing in `'objec@samples'`,
this column will get overwritten, retrieving a warning message.

## Usage

``` r
taxa2samples(object, ...)

# S3 method for class 'vegtable'
taxa2samples(
  object,
  merge_to,
  include_levels,
  add_relations = FALSE,
  add_traits = FALSE,
  ...
)
```

## Arguments

- object:

  A
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object.

- ...:

  Further arguments passed among methods.

- merge_to:

  Character value indicating the level (taxonomic rank) to which taxa of
  lower rank have to be merged.

- include_levels:

  Character vector indicating the levels to be considered in the output
  object. This will set the values of **TaxonConceptID** and any
  respective values inserted from slots **taxonRelations** and
  **taxonTraits** as NA.

- add_relations:

  A logical value indicating whether the content of slot
  **taxonRelations** have to be inserted in slot **samples** or not.

- add_traits:

  A logical value indicating whether the content of slot **taxonTraits**
  have to be inserted in slot **samples** or not.

## Value

An object of class
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Add only variable TaxonConceptID
veg <- taxa2samples(Kenya_veg)
head(veg@samples)
#>   TaxonUsageID ReleveID COVER_CODE LAYER SOCIABILIT INDIVID br_bl b_bbds
#> 1           18      358          +     0       <NA>    <NA>     +   <NA>
#> 2           18      470          +     0       <NA>    <NA>     +   <NA>
#> 3           18     1634          +     0       <NA>    <NA>     +   <NA>
#> 4           18      587          +     0       <NA>    <NA>     +   <NA>
#> 5           18     1949         2m     0       <NA>    <NA>  <NA>     2m
#> 6           18     1777          +     0       <NA>    <NA>     +   <NA>
#>   TaxonConceptID
#> 1             18
#> 2             18
#> 3             18
#> 4             18
#> 5             18
#> 6             18

## Add also information from slots taxonRelations and taxonTraits
veg <- taxa2samples(Kenya_veg, add_relations = TRUE, add_traits = TRUE)
head(veg@samples)
#>   TaxonUsageID ReleveID COVER_CODE LAYER SOCIABILIT INDIVID br_bl b_bbds
#> 1           18      358          +     0       <NA>    <NA>     +   <NA>
#> 2           18      470          +     0       <NA>    <NA>     +   <NA>
#> 3           18     1634          +     0       <NA>    <NA>     +   <NA>
#> 4           18      587          +     0       <NA>    <NA>     +   <NA>
#> 5           18     1949         2m     0       <NA>    <NA>  <NA>     2m
#> 6           18     1777          +     0       <NA>    <NA>     +   <NA>
#>   TaxonConceptID Parent   Level ViewID  uri Basionym AcceptedName
#> 1             18  54759 species      1 <NA>       NA           18
#> 2             18  54759 species      1 <NA>       NA           18
#> 3             18  54759 species      1 <NA>       NA           18
#> 4             18  54759 species      1 <NA>       NA           18
#> 5             18  54759 species      1 <NA>       NA           18
#> 6             18  54759 species      1 <NA>       NA           18
#>         lf_behn_2018
#> 1 facultative_annual
#> 2 facultative_annual
#> 3 facultative_annual
#> 4 facultative_annual
#> 5 facultative_annual
#> 6 facultative_annual

## Different ranks recorded at samples
veg <- taxa2samples(Kenya_veg, add_relations = TRUE)
summary(veg@samples$Level)
#>       form    variety subspecies    species    complex      genus     family 
#>          0       2489       2453      68685          0        914          0 

## Aggregate taxa to family level
veg <- taxa2samples(Kenya_veg, add_relations = TRUE, merge_to = "family")
summary(veg@samples$Level)
#>       form    variety subspecies    species    complex      genus     family 
#>          0          0          0          0          0          0      74531 
#>       NA's 
#>         10 
```
