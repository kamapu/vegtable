# Retrieve synonyms or taxon concepts used in a data set

Plots records are rather linked to plant names than plant taxon concepts
and `used_synonyms()` lists all synonyms linked to records in a
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object, including their respective accepted names.

On the other side, the function `used_concepts()` produces a subset of
the taxonomic list embeded in the slot **species** including only
taxonomic concepts linked to records in the slot **samples**.

## Usage

``` r
used_synonyms(x, ...)

# S3 method for class 'vegtable'
used_synonyms(x, ...)

used_concepts(x, ...)

# S3 method for class 'vegtable'
used_concepts(
  x,
  keep_children = FALSE,
  keep_parents = FALSE,
  keep_synonyms = TRUE,
  ...
)
```

## Arguments

- x:

  A
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object.

- ...:

  Further arguments to be passed from or to another methods.

- keep_children:

  A logical argument indicating whether children of selected taxa should
  be included in the output or not. This argument passed to
  [`taxlist::get_children()`](https://docs.ropensci.org/taxlist/reference/get_children.html).

- keep_parents:

  A logical value indicating whether parents of selected taxa should be
  included in the output or not. This argument passed to
  [`taxlist::get_parents()`](https://docs.ropensci.org/taxlist/reference/get_children.html).

- keep_synonyms:

  A logical value indicating whether synonyms should be included or not.

## Value

The function `used_synonyms()` returns a data frame including following
variables:

- SynonymID:

  ID of the taxon usage name applied as synonym.

- Synonym:

  The synonym itself.

- SynonymAuthor:

  Author of synonym.

- TaxonConceptID:

  ID of the respective taxon concept.

- AcceptedNameID:

  ID of the taxon usage name set as accepted name of the taxon concept.

- AcceptedName:

  The respective accepted name.

- AcceptedNameAuthor:

  The author of the accepted name.

The function `used_concepts()` returns a
[taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
object including only taxa occurring in the plot observations of the
input
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
object.

## See also

[`taxlist::accepted_name()`](https://docs.ropensci.org/taxlist/reference/accepted_name.html)

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Synonyms used in the Kenya_veg
Synonyms <- used_synonyms(Kenya_veg)
head(Synonyms)
#>   SynonymID                                  Synonym   SynonymAuthor
#> 1     50168                  Clerodendron johnstonii              NA
#> 2     50222 Cyperus sesquiflorus ssp. appendiculatus (K. Schum.) Lye
#> 3     50277                           Teclea nobilis          Delile
#> 4     50297               Polystichum fuscopaleaceum          Alston
#> 5     50304                        Dombeya goetzenii       K. Schum.
#> 6     50225                 Solanum sessilistellatum          Bitter
#>   TaxonConceptID AcceptedNameID            AcceptedName AcceptedNameAuthor
#> 1          50167          50167 Clerodendrum johnstonii              Oliv.
#> 2          50221          50221  Cyperus afrosylvestris                Lye
#> 3          50276          50276          Vepris nobilis    (Delile) Mziray
#> 4          50296          50296   Polystichum discretum    (D. Don) J. Sm.
#> 5          50303          50303         Dombeya torrida (J.F. Gmel.) Bamps
#> 6          50224          50224  Solanum nigriviolaceum             Bitter

## Subset species list to used concepts
species <- used_concepts(Kenya_veg)
Kenya_veg@species
#> object size: 554 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of taxon usage names: 3164 
#> number of taxon concepts: 2392 
#> trait entries: 102 
#> number of trait variables: 1 
#> taxon views: 3 
#> 
#> concepts with parents: 2237 
#> concepts with children: 957 
#> 
#> concepts with rank information: 2392 
#> concepts without rank information: 0 
#> 
#> family: 154
#>   genus: 699
#>     complex: 0
#>       species: 1422
#>         subspecies: 52
#>           variety: 65
#>             form: 0
#> 
#> 
species
#> object size: 395.8 Kb 
#> validation of 'taxlist' object: TRUE 
#> 
#> number of taxon usage names: 2339 
#> number of taxon concepts: 1573 
#> trait entries: 97 
#> number of trait variables: 1 
#> taxon views: 3 
#> 
#> concepts with parents: 514 
#> concepts with children: 138 
#> 
#> concepts with rank information: 1573 
#> concepts without rank information: 0 
#> 
#> family: 0
#>   genus: 90
#>     complex: 0
#>       species: 1366
#>         subspecies: 52
#>           variety: 65
#>             form: 0
#> 
#> 
```
