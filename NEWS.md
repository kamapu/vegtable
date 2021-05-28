vegtable 0.1.7
==============

## New Features

* New function `used_concepts()` to extract taxon concepts that are occurring
in the plot observations.

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
* Function `transform()` deprecated and replaced by `cover_trans()`.

## Bug Fixes

* Files written by `write_juice()` in Linux are now readable in Windows.
* In `clean()` objects with no entries resulted in invalid objects.
* Bug in `count_taxa()` for aggregating ranks with missing records for lower taxa

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
