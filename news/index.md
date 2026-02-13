# Changelog

## vegtable 0.2.0

### New Features

- New data set `veg_layers` with information about vegetation layers.
- New function
  [`new_layer()`](http://kamapu.github.io/vegtable/reference/new_layer.md)
  to add layer information tables in vegtable objects.
- A formula method for
  [`cross2db()`](http://kamapu.github.io/vegtable/reference/crosstable.md).

### Improvements

- Function
  [`count_taxa()`](http://kamapu.github.io/vegtable/reference/count_taxa.md)
  also inserts results into slot **relations**.
- New arguments `'level'` and `'include_lower'` in function
  [`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md)
  They enable selection and/or merging of taxon ranks in cross tables.
- Function
  [`cross2db()`](http://kamapu.github.io/vegtable/reference/crosstable.md)
  implemented in two methods, a `data.frame-method` and a
  `matrix-method`.
- New parameter `keep_synonyms` in function
  [`used_concepts()`](http://kamapu.github.io/vegtable/reference/used_synonyms.md),
  which can be used to skp synonyms in the output object.

### Bug fixes

- Fuction
  [`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md)
  was not applying properly the argument **use_nas**.
- Errors in function
  [`cross2db()`](http://kamapu.github.io/vegtable/reference/crosstable.md)
  were solved. In this process became this function additional
  arguments.

## vegtable 0.1.8

CRAN release: 2023-03-14

### New Features

- New function
  [`veg_diversity()`](http://kamapu.github.io/vegtable/reference/veg_diversity.md)
  and functions calculating diversity indices:
  [`shannon()`](http://kamapu.github.io/vegtable/reference/veg_diversity.md),
  [`evenness()`](http://kamapu.github.io/vegtable/reference/veg_diversity.md),
  [`dominance()`](http://kamapu.github.io/vegtable/reference/veg_diversity.md),
  [`simpson()`](http://kamapu.github.io/vegtable/reference/veg_diversity.md),
  and
  [`richness()`](http://kamapu.github.io/vegtable/reference/veg_diversity.md).
- New function
  [`new_relation()`](http://kamapu.github.io/vegtable/reference/new_relation.md)
  inserting new relations into `vegtable` objects. This function is
  internally called by
  [`veg_relation()`](http://kamapu.github.io/vegtable/reference/veg_relation.md).
- New function
  [`df2coverconvert()`](http://kamapu.github.io/vegtable/reference/df2coverconvert.md)
  coercing data frames and lists into `coverconvert` objects.

### Improvements

- Slot **relations** may deal with any element that can be coerced to
  `data.frame`.
- All elements in slot **syntax** have to be of class `taxlist`
- Parameter `in_header` defined in several functions is set as
  `in_header = TRUE`.
- Former method for ‘aggregate()’ is now defined in function
  ‘veg_aggregate()’.
- Deprecated functions:
  [`match_names()`](https://docs.ropensci.org/taxlist/reference/match_names.html),
  [`merge_taxa()`](https://docs.ropensci.org/taxlist/reference/merge_taxa.html).
  These functions are now exclusive for objects of class `taxlist`.
- Method
  [`names()`](http://kamapu.github.io/vegtable/reference/names.md) for
  `vegtable` objects will retrieve a list with all names from the
  respective slots.
- New coercion methods including functions
  [`as()`](http://kamapu.github.io/vegtable/reference/coerce-methods.md)
  and `as<-`.
- Functions
  [`trait_proportion()`](http://kamapu.github.io/vegtable/reference/trait_stats.md)
  and
  [`trait_stats()`](http://kamapu.github.io/vegtable/reference/trait_stats.md)
  are also adding results to the slot **relations**.

### Bug Fixes

- An issue was solved for function
  [`taxa2samples()`](http://kamapu.github.io/vegtable/reference/taxa2samples.md)
  when setting an argument in the parameter `'merge_to'`, which was not
  properly working in all cases.

## vegtable 0.1.7

CRAN release: 2021-10-13

### New Features

- New function
  [`used_concepts()`](http://kamapu.github.io/vegtable/reference/used_synonyms.md)
  to extract taxon concepts that are occurring in the plot observations.
- New function
  [`cover_trans()`](http://kamapu.github.io/vegtable/reference/cover_trans.md)
  in replacement of
  [`transform()`](https://rdrr.io/r/base/transform.html)
- New slot `syntax` in `vegtable` objects.
- `show()` and
  [`print()`](https://docs.ropensci.org/taxlist/reference/summary.html)
  methods for objects `vegtable`, `coverconvert`, and `shaker`.

### Improvements

- In function
  [`make_cocktail()`](http://kamapu.github.io/vegtable/reference/make_cocktail.md),
  names of syntaxa are allowed to start with numerical values.
- Function
  [`write_juice()`](http://kamapu.github.io/vegtable/reference/write_juice.md)
  includes a messages when working and with some metadata for
  cross-check when importing in Juice.
- New arguments in function
  [`taxa2samples()`](http://kamapu.github.io/vegtable/reference/taxa2samples.md)
  setting the levels to be included in output object (`include_levels`)
  and to remove taxa without levels (`na.rm`).
- Application of
  [`subset()`](http://kamapu.github.io/vegtable/reference/subset.md)
  extended to content in slot **relations**.
- Function `vegtable2kml()` was deprecated to resolve dependencies on
  GIS packages associated to package `plotKML`.
- Validity checks allow plots in slot **header** without records in
  **samples**.
- Function
  [`summary()`](http://kamapu.github.io/vegtable/reference/summary.md)
  also providing number of plots with records.
- Function [`transform()`](https://rdrr.io/r/base/transform.html)
  deprecated and replaced by
  [`cover_trans()`](http://kamapu.github.io/vegtable/reference/cover_trans.md).

### Bug Fixes

- Files written by
  [`write_juice()`](http://kamapu.github.io/vegtable/reference/write_juice.md)
  in Linux are now readable in Windows.
- In [`clean()`](http://kamapu.github.io/vegtable/reference/clean.md)
  objects with no entries resulted in invalid objects.
- Bug in
  [`count_taxa()`](http://kamapu.github.io/vegtable/reference/count_taxa.md)
  for aggregating ranks with missing records for lower taxa.

## vegtable 0.1.6

CRAN release: 2020-04-30

#### Improvements

- Documentation implemented in `roxygen2`.
- New argument `preserve_ids` in function
  [`add_releves()`](http://kamapu.github.io/vegtable/reference/add_releves.md).

#### Bug Fixes

- Function
  [`count_taxa()`](http://kamapu.github.io/vegtable/reference/count_taxa.md)
  adapted to the current package `taxlist`.

## vegtable 0.1.5

CRAN release: 2020-01-13

#### New Features

- New function
  [`relation2header()`](http://kamapu.github.io/vegtable/reference/relation2heder.md).
- New functions
  [`trait_stats()`](http://kamapu.github.io/vegtable/reference/trait_stats.md)
  and
  [`trait_proportion()`](http://kamapu.github.io/vegtable/reference/trait_stats.md).

#### Improvements

- New argument `add_traits` in function
  [`taxa2samples()`](http://kamapu.github.io/vegtable/reference/taxa2samples.md).
- Output of
  [`count_taxa()`](http://kamapu.github.io/vegtable/reference/count_taxa.md),
  formula-method, indicates the counted rank in the output.
- Adjusted settings in function
  [`write_juice()`](http://kamapu.github.io/vegtable/reference/write_juice.md)
  for general cases.
- Argument `as_matrix` in function
  [`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md)
  alternating `matrix` or `data.frame` as output.
- New arguments `suffix` and `in_header` for the function
  [`count_taxa()`](http://kamapu.github.io/vegtable/reference/count_taxa.md).

#### Bug Fixes

- Function
  [`taxa2samples()`](http://kamapu.github.io/vegtable/reference/taxa2samples.md)
  was not properly working when `taxlist` had taxon concepts without
  level information.

## vegtable 0.1.4

CRAN release: 2019-01-22

#### New Features

- New data set `aspect_conv`.
- New function
  [`taxa2samples()`](http://kamapu.github.io/vegtable/reference/taxa2samples.md)
  for changing record names by their respective accepted names.
- Function
  [`count_taxa()`](http://kamapu.github.io/vegtable/reference/count_taxa.md)
  implemented for `vegtable` objects.

#### Improvements

- Updated data set `Kenya_veg` Including hierarchical ranks.

#### Bugs

- Function `cross2db` was not working properly for tables with only one
  column

## vegtable 0.1.3

CRAN release: 2018-06-29

#### Improvements

- A method for `numeric` values in function
  [`transform()`](https://rdrr.io/r/base/transform.html).

## vegtable 0.1.2

CRAN release: 2018-05-10

#### New Features

- File **inst/ChangeLog** replaced by **NEWS.md**.
- Method
  [`match_names()`](https://docs.ropensci.org/taxlist/reference/match_names.html)
  for `vegtable` objects.
- New method
  [`layers2samples()`](http://kamapu.github.io/vegtable/reference/layers2samples.md).
- New method
  [`add_releves()`](http://kamapu.github.io/vegtable/reference/add_releves.md),
  adding releves from data frames into `vegtable` objects.

#### Improvements

- No column `LayerID` in prototype for `vegtable` objects.
- `NA` values are allowed for layers in slot `samples`.
- Layers are not any more restricted to single plots.
- Orphaned entries in slot `layers` will be deleted by function
  [`clean()`](http://kamapu.github.io/vegtable/reference/clean.md).
- Argument `match_header` included in function
  [`veg_relation()`](http://kamapu.github.io/vegtable/reference/veg_relation.md).
- Validity check is not accepting `NA` values in `TaxonUsageID` at slot
  `samples`.
- Validity check tests also the values of relations matching header with
  the respective relation.

## vegtable 0.1.1

CRAN release: 2018-02-08

#### New Features

- A method for function `aggregate`.
- Slot `layers` added to `vegtable` objects.

#### Improvements

- Method `subset` working similarly to
  [`taxlist::subset`](https://docs.ropensci.org/taxlist/reference/subset.html).
- Function `clean` does not modify slot `species`.
- Function `crosstable` access also to taxon traits.
- New argument `use_nas` for function `crosstable`.

## vegtable 0.1.0

CRAN release: 2017-08-08

#### New Features

- First delivery in CRAN.
