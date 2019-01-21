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
