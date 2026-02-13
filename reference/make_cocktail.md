# Produce a Cocktail classification

Classification of
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
objects according to **Cocktail** algorithms.

Cocktail algorithms are logical functions selecting plots according to
either occurrence of species groups and cover values of single species.
A group will be declared as occurring in a plot when at least a half of
its members is present in the plot.

This function inserts single columns with logical values indicating
whether a plot is classified in the vegetation unit or not. An
additional column (name provided in argument `syntax`) compile all
vegetation units, indicating with a `+` symbol those plots classified in
more than one vegetation unit. When only a part of the formulas will be
used, it should be specified by the argument `which`.

These functions are implemented for constructing or complementing
[shaker](http://kamapu.github.io/vegtable/reference/shaker-class.md)
objects. Note that construction of those objects will always require a
`companion` object, which is either an object of class
[taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
or
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

## Usage

``` r
set_group(shaker, companion, group, ...)

# S4 method for class 'shaker,taxlist,character'
set_group(
  shaker,
  companion,
  group,
  group_id,
  authority = FALSE,
  enc_cont = "latin1",
  enc_gr = "utf8",
  ...
)

# S4 method for class 'shaker,vegtable,character'
set_group(shaker, companion, group, ...)

set_pseudo(shaker, companion, pseudo, ...)

# S4 method for class 'shaker,taxlist,character'
set_pseudo(
  shaker,
  companion,
  pseudo,
  pseudo_id,
  authority = FALSE,
  enc_cont = "latin1",
  enc_gr = "utf8",
  ...
)

# S4 method for class 'shaker,vegtable,character'
set_pseudo(shaker, companion, pseudo, ...)

set_formula(shaker, companion, formula, ...)

# S4 method for class 'shaker,taxlist,character'
set_formula(
  shaker,
  companion,
  formula,
  formula_id,
  authority = FALSE,
  enc_cont = "latin1",
  enc_gr = "utf8",
  ...
)

# S4 method for class 'shaker,vegtable,character'
set_formula(shaker, companion, formula, ...)

make_cocktail(shaker, vegtable, ...)

# S4 method for class 'shaker,vegtable'
make_cocktail(
  shaker,
  vegtable,
  which,
  cover,
  syntax = "Syntax",
  FUN = sum,
  in_header = TRUE,
  ...
)
```

## Arguments

- shaker:

  An object of class
  [shaker](http://kamapu.github.io/vegtable/reference/shaker-class.md)
  containing the respective cocktail definitions.

- companion:

  Either a
  [taxlist::taxlist](https://docs.ropensci.org/taxlist/reference/taxlist-class.html)
  or a
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object.

- ...:

  Further arguments passes from or to other methods.

- authority:

  Logical value indicating whether author names should be included in
  the taxon name or not.

- enc_cont, enc_gr:

  Encodings used for special characters.

- pseudo, group:

  Character vector with names of taxa included in a pseudo-species or a
  species group.

- pseudo_id, group_id, formula_id:

  Character value as name of the pseudo-species, species group or
  defined vegetation unit.

- formula:

  Character vector including a formula as definition of a vegetation
  unit.

- vegtable:

  An object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  containing the vegetation observations to be classified.

- which:

  Integer or character indicating the definition to be applied for
  classification.

- cover:

  Name of the cover variable in `vegtable`.

- syntax:

  Character value indicating the name of the retrieved variable
  including the final classification of plots.

- FUN:

  Function used for merging multiple occurrence of species in a single
  plot.

- in_header:

  Logical value indicating whether results of Cocktail classification
  should be inserted to the header of the input vegtable or not. In the
  second case, a data frame is provided as output.

## Value

A data frame corresponding to the slot `header` of input object
`vegtable`, including the results of Cocktail classification for the
respective plots.

A [shaker](http://kamapu.github.io/vegtable/reference/shaker-class.md)
object.

## References

**Alvarez M (2017).** Classification of aquatic and semi-aquatic
vegetation in two East African sites: Cocktail definitions and
syntaxonomy. *Phytocoenologia*.

**Bruelheide H (2000).** A new measure of fidelity and its application
to defining species groups. *Journal of Vegetation Science* 11: 167–178.

**Kočí M, Chytrý M, Tichý L (2003).** Formalized reproduction of an
expert-based phytosociological classification: a case study of subalpine
tall-forb vegetation. *Journal of Vegetation Science* 14: 601–610.

## See also

[shaker](http://kamapu.github.io/vegtable/reference/shaker-class.md)
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
[Wetlands](http://kamapu.github.io/vegtable/reference/Wetlands-data.md)

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
## Example from Alvarez (2017)
Wetlands_veg <- make_cocktail(Wetlands, Wetlands_veg, cover = "percen")
summary(as.factor(Wetlands_veg@header$Syntax))
#>    +  HE1  HE2  HE3  HE4  HE5  HE6  HE7  HY1  HY2 NA's 
#>    1   10   10   10    6    8    4    2   10    8   31 

## Same but only for two vegetation units
Wetlands_veg <- make_cocktail(Wetlands, Wetlands_veg,
  which = c("HY1", "HY2"), cover = "percen"
)
summary(as.factor(Wetlands_veg$Syntax))
#>  HY1  HY2 NA's 
#>   10    8   82 

## Construct the 'shaker' object anew
Wetlands <- new("shaker")

## Set a pseudo-species
Wetlands <- set_pseudo(Wetlands, Wetlands_veg, c(
  "Cyperus latifolius",
  "Cyperus exaltatus"
))

## Set a species group
Wetlands <- set_group(Wetlands, Wetlands_veg,
  group_id = "Cyperus papyrus",
  group = c(
    "Cyperus papyrus",
    "Cyclosorus interruptus",
    "Lepistemon owariense"
  )
)

## Set a fromula
Wetlands <- set_formula(Wetlands, Wetlands_veg,
  formula_id = "HE1",
  formula = "groups:'Cyperus papyrus' | species:'Cyperus papyrus > 50'"
)

## Summaries
summary(Wetlands)
#> Number of pseudo-species: 1 
#> Number of species groups: 1 
#> Number of formulas: 1 
summary(Wetlands, Wetlands_veg)
#> ## Pseudo-species: 
#> * 'Cyperus latifolius' contains: 
#>      Cyperus exaltatus 
#> 
#> ## Species groups: 
#> * 'Cyperus papyrus' group: 
#>      Cyperus papyrus 
#>      Cyclosorus interruptus 
#>      Lepistemon owariense 
#> 
#> ## Formulas: 
#> * HE1: groups:'Cyperus papyrus' | species:'Cyperus papyrus > 50' 
#> 
```
