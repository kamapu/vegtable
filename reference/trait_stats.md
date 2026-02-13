# Statistics and proportion for taxon traits

Calculation of statistics and proportions of taxon traits for plot
observations or groups of observations, considering data relationships,
taxonomic ranks and the handling of not available values.

In `trait_stats()` you can use customized functions, which have to be
defined as `foo(x, w, ...)`, where `'x'` is the (numeric) taxon trait
and `'w'` is the weight (e.g. the abundance).

With the arguments `taxon_levels` and `merge_to` the used taxonomic
ranks can be defined, where the first one indicates which ranks have to
be considered in the calculations and the second one determine the
aggregation of taxa from a lower level to a parental one.

## Usage

``` r
trait_stats(trait, object, ...)

# S4 method for class 'character,vegtable'
trait_stats(
  trait,
  object,
  FUN,
  head_var = "ReleveID",
  taxon_levels,
  merge_to,
  weight,
  suffix = "_stats",
  in_header = TRUE,
  ...
)

# S4 method for class 'formula,vegtable'
trait_stats(trait, object, ...)

trait_proportion(trait, object, ...)

# S4 method for class 'character,vegtable'
trait_proportion(
  trait,
  object,
  head_var = "ReleveID",
  trait_levels,
  taxon_levels,
  merge_to,
  include_nas = TRUE,
  weight,
  suffix = "_prop",
  in_header = TRUE,
  ...
)

# S4 method for class 'formula,vegtable'
trait_proportion(trait, object, ...)
```

## Arguments

- trait:

  Either a character value indicating the name of trait variable or a
  formula as `'trait ~ head_var'`. Note that you can add multiple
  variables in the form `trait_1 + ... + trait_n ~ head_var`.

- object:

  A
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
  object.

- ...:

  Further arguments passed among methods. In the case of the character
  method, they are passed to 'FUN'.

- FUN:

  A function usually defined as `foo(x)` or as `foo(x, w)` for weighted
  statistics.

- head_var:

  Character value, the name of the variable at slot header to be used as
  aggregation level for the calculation of statistics or proportions. If
  not provided, the function will use **ReleveID** by default.

- taxon_levels:

  Character vector indicating the selected taxonomic ranks to be
  considered in the output.

- merge_to:

  Character value indicating the taxonomic rank for aggregation of taxa.
  All ranks lower than the one indicated here will be assigned to the
  respective parents at the required taxonomic rank.

- weight:

  Character value indicating the name of the variable at slot
  **samples** used as weight for the proportions. Usually the numeric
  abundance.

- suffix:

  A suffix added to the name of the trait variable or to the levels of
  categorical trait variables. I is meant to avoid homonymous variables
  within the same object.

- in_header:

  Logical value indicating whether the output should be inserted in the
  slot **header** or provided as data frame. In the case that
  `'head_var'` (or the right term in the formula method) is different
  from **ReleveID**, the statistics and proportions will be inserted in
  the respective data frame at slot **relations**.

- trait_levels:

  Character vector indicating a selection of levels from a trait, in the
  case that some levels should be ignored in the output. Trait levels
  that are skipped at output will be still used for the calculation of
  proportions. This argument gets only applied for the character method.

- include_nas:

  Logical value indicating whether NAs should be considered for the
  calculation of proportions or not.

## Value

A data frame with the proportions of traits levels or statistics for the
trait variable, or an object of class
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
including those results at the slot `header`.

## Author

Miguel Alvarez <kamapu78@gmail.com>

## Examples

``` r
veg <- cover_trans(Kenya_veg, to = "cover")
veg <- trait_proportion("lf_behn_2018", veg,
  trait_levels = "obligate_annual", weight = "cover", include_nas = FALSE
)
summary(veg$obligate_annual_prop)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#> 0.00769 0.25000 0.50000 0.49922 0.71429 1.00000    1039 
```
