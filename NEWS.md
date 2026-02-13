# vegtable 0.2.0

## New Features

- New data set `veg_layers` with information about vegetation layers.
- New function `new_layer()` to add layer information tables in vegtable
  objects.
- A formula method for `cross2db()`.

## Improvements

- Function `count_taxa()` also inserts results into slot **relations**.
- New arguments `'level'` and `'include_lower'` in function `crosstable()`
  They enable selection and/or merging of taxon ranks in cross tables.
- Function `cross2db()` implemented in two methods, a `data.frame-method`
  and a `matrix-method`.
- New parameter `keep_synonyms` in function `used_concepts()`, which can be used
  to skp synonyms in the output object.

## Bug fixes

- Fuction `crosstable()` was not applying properly the argument **use_nas**.
- Errors in function `cross2db()` were solved. In this process became this
  function additional arguments.

vegtable 0.1.8
==============

## New Features

* New function `veg_diversity()` and functions calculating diversity indices:
  `shannon()`, `evenness()`, `dominance()`, `simpson()`, and `richness()`.
* New function `new_relation()` inserting new relations into `vegtable` objects.
  This function is internally called by `veg_relation()`.
* New function `df2coverconvert()` coercing data frames and lists into
  `coverconvert` objects.

## Improvements

* Slot **relations** may deal with any element that can be coerced to
  `data.frame`.
* All elements in slot **syntax** have to be of class `taxlist`
* Parameter `in_header` defined in several functions is set as
  `in_header = TRUE`.
* Former method for 'aggregate()' is now defined in function 'veg_aggregate()'.
* Deprecated functions: `match_names()`, `merge_taxa()`. These functions are now
  exclusive for objects of class `taxlist`.
* Method `names()` for `vegtable` objects will retrieve a list with all names
  from the respective slots.
* New coercion methods including functions `as()` and `as<-`.
* Functions `trait_proportion()` and `trait_stats()` are also adding results to
  the slot **relations**.
  
## Bug Fixes

* An issue was solved for function `taxa2samples()` when setting an argument in
  the parameter `'merge_to'`, which was not properly working in all cases.

vegtable 0.1.7
==============

## New Features

* New function `used_concepts()` to extract taxon concepts that are occurring
in the plot observations.
* New function `cover_trans()` in replacement of `transform()`
* New slot `syntax` in `vegtable` objects.
* `show()` and `print()` methods for objects `vegtable`, `coverconvert`,
  and `shaker`.

## Improvements

* In function `make_cocktail()`, names of syntaxa are allowed to start with numerical values.
* Function `write_juice()` includes a messages when working and with some
metadata for cross-check when importing in Juice.
* New arguments in function `taxa2samples()` setting the levels to be included
in output object (`include_levels`) and to remove taxa without levels
(`na.rm`).
* Application of `subset()` extended to content in slot **relations**.
* Function `vegtable2kml()` was deprecated to resolve dependencies on GIS
  packages associated to package `plotKML`.
* Validity checks allow plots in slot **header** without records in **samples**.
* Function `summary()` also providing number of plots with records.
* Function `transform()` deprecated and replaced by `cover_trans()`.

## Bug Fixes

* Files written by `write_juice()` in Linux are now readable in Windows.
* In `clean()` objects with no entries resulted in invalid objects.
* Bug in `count_taxa()` for aggregating ranks with missing records for lower
  taxa.

vegtable 0.1.6
==============

### Improvements

* Documentation implemented in `roxygen2`.
* New argument `preserve_ids` in function `add_releves()`.

### Bug Fixes

* Function `count_taxa()` adapted to the current package `taxlist`.

vegtable 0.1.5
==============

### New Features

* New function `relation2header()`.
* New functions `trait_stats()` and `trait_proportion()`.

### Improvements

* New argument `add_traits` in function `taxa2samples()`.
* Output of `count_taxa()`, formula-method, indicates the counted rank in the output.
* Adjusted settings in function `write_juice()` for general cases.
* Argument `as_matrix` in function `crosstable()` alternating `matrix` or `data.frame` as output.
* New arguments `suffix` and `in_header` for the function `count_taxa()`.

### Bug Fixes

* Function `taxa2samples()` was not properly working when `taxlist` had taxon concepts without level information.

vegtable 0.1.4
==============

### New Features

* New data set `aspect_conv`.
* New function `taxa2samples()` for changing record names by their respective accepted names.
* Function `count_taxa()` implemented for `vegtable` objects.

### Improvements

* Updated data set `Kenya_veg` Including hierarchical ranks.

### Bugs

* Function `cross2db` was not working properly for tables with only one column

vegtable 0.1.3
==============

### Improvements

* A method for `numeric` values in function `transform()`.

vegtable 0.1.2
==============

### New Features

* File **inst/ChangeLog** replaced by **NEWS.md**.
* Method `match_names()` for `vegtable` objects.
* New method `layers2samples()`.
* New method `add_releves()`, adding releves from data frames into `vegtable` objects.

### Improvements
* No column `LayerID` in prototype for `vegtable` objects.
* `NA` values are allowed for layers in slot `samples`.
* Layers are not any more restricted to single plots.
* Orphaned entries in slot `layers` will be deleted by function `clean()`.
* Argument `match_header` included in function `veg_relation()`.
* Validity check is not accepting `NA` values in `TaxonUsageID` at slot `samples`.
* Validity check tests also the values of relations matching header with the respective relation.

vegtable 0.1.1
==============

### New Features

* A method for function `aggregate`.
* Slot `layers` added to `vegtable` objects.

### Improvements

* Method `subset` working similarly to `taxlist::subset`.
* Function `clean` does not modify slot `species`.
* Function `crosstable` access also to taxon traits.
* New argument `use_nas` for function `crosstable`.

vegtable 0.1.0
==============

### New Features

* First delivery in CRAN.
