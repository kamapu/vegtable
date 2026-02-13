# Subset functions for vegtable objects

Produce subsets of
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
objects.

Logical operations can be applied either to the plots, or the relations,
which are the main slots in that class.

This method can be referred to the slot `species` the same way as
[`taxlist::subset()`](https://docs.ropensci.org/taxlist/reference/subset.html),
then the rest of the data will include only references to the subset of
species list.

## Usage

``` r
# S4 method for class 'vegtable'
subset(
  x,
  subset,
  slot = "header",
  keep_children = FALSE,
  keep_parents = FALSE,
  relation,
  ...
)
```

## Arguments

- x:

  A
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object for subset.

- subset:

  Logical expression for the subset.

- slot:

  Character value indicating the slot used as reference for subset. At
  the moment only the values "taxonNames", "taxonRelations",
  "taxonTraits", "header", "samples", and "relations" are accepted. The
  three first values will be applied to the respective slots in the
  contained
  [taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
  object (slot **species**).

- keep_children:

  Argument passed to
  [`taxlist::get_children()`](https://docs.ropensci.org/taxlist/reference/get_children.html).

- keep_parents:

  Argument passed to
  [`taxlist::get_parents()`](https://docs.ropensci.org/taxlist/reference/get_children.html).

- relation:

  Character value indicating the relation (slot **relations**) to be
  used as reference for subset.

- ...:

  Further arguments passed from or to other methods.

## Value

A S4 object of class
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Subset by taxon name
Kenya_sub <- subset(
  x = Kenya_veg, subset = TaxonName == "Tagetes",
  slot = "taxonNames", keep_children = TRUE, keep_parents = TRUE
)
summary(Kenya_sub)
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 803 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 1946 
#>    plots with records: 48 
#>    variables in header: 34 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 3 
#>    taxon concepts: 3 
#>    validity: TRUE 
#> 
summary(Kenya_sub@species)
#> object size: 7.5 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of taxon usage names: 3 
#> number of taxon concepts: 3 
#> trait entries: 1 
#> number of trait variables: 1 
#> taxon views: 3 
#> 
#> concepts with parents: 2 
#> concepts with children: 2 
#> 
#> concepts with rank information: 3 
#> concepts without rank information: 0 
#> 
#> family: 1
#>   genus: 1
#>     complex: 0
#>       species: 1
#>         subspecies: 0
#>           variety: 0
#>             form: 0
#> 
#> 

## Subset by taxon relations
Kenya_sub <- subset(
  x = Kenya_veg, subset = Level == "species",
  slot = "taxonRelations"
)
summary(Kenya_sub)
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 8657.9 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 1946 
#>    plots with records: 1946 
#>    variables in header: 34 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 2089 
#>    taxon concepts: 1422 
#>    validity: TRUE 
#> 
summary(Kenya_sub@species)
#> object size: 353 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of taxon usage names: 2089 
#> number of taxon concepts: 1422 
#> trait entries: 102 
#> number of trait variables: 1 
#> taxon views: 3 
#> 
#> concepts with rank information: 1422 
#> concepts without rank information: 0 
#> 
#> family: 0
#>   genus: 0
#>     complex: 0
#>       species: 1422
#>         subspecies: 0
#>           variety: 0
#>             form: 0
#> 
#> 

## Subset by taxon traits
Kenya_sub <- subset(
  x = Kenya_veg, subset = lf_behn_2018 == "obligate_annual",
  slot = "taxonTraits"
)
summary(Kenya_sub)
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 965.9 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 1946 
#>    plots with records: 907 
#>    variables in header: 34 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 121 
#>    taxon concepts: 32 
#>    validity: TRUE 
#> 
summary(Kenya_sub@species)
#> object size: 26.8 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of taxon usage names: 121 
#> number of taxon concepts: 32 
#> trait entries: 32 
#> number of trait variables: 1 
#> taxon views: 3 
#> 
#> concepts with rank information: 32 
#> concepts without rank information: 0 
#> 
#> family: 0
#>   genus: 0
#>     complex: 0
#>       species: 32
#>         subspecies: 0
#>           variety: 0
#>             form: 0
#> 
#> 

## Subset by header
Kenya_sub <- subset(x = Kenya_veg, subset = ALTITUDE <= 1000, slot = "header")
summary(Kenya_sub)
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 725.2 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 16 
#>    plots with records: 16 
#>    variables in header: 27 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 3164 
#>    taxon concepts: 2392 
#>    validity: TRUE 
#> 

## Subset by samples (after converting coverage)
Kenya_veg <- cover_trans(x = Kenya_veg, to = "cover_percentage", rule = "middle")
Kenya_sub <- subset(x = Kenya_veg, subset = cover_percentage >= 50, slot = "samples")
summary(Kenya_sub)
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 1489.8 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 1946 
#>    plots with records: 1084 
#>    variables in header: 34 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 3164 
#>    taxon concepts: 2392 
#>    validity: TRUE 
#> 

## Subset by relations
Kenya_sub <- subset(
  x = Kenya_veg, subset = as.integer(YEAR) >= 2000,
  slot = "relations", relation = "REFERENCE"
)
summary(Kenya_sub)
#> ## Metadata 
#>    db_name: Sweadataveg
#>    sp_list: Easplist
#>    dictionary: Swea
#>    object size: 2674.5 Kb 
#>    validity: TRUE 
#> 
#> ## Content 
#>    number of plots: 318 
#>    plots with records: 318 
#>    variables in header: 30 
#>    number of relations: 3 
#> 
#> ## Taxonomic List 
#>    taxon names: 3164 
#>    taxon concepts: 2392 
#>    validity: TRUE 
#> 
```
