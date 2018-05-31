vegtable 0.1.2
==============

### New Features

* File **inst/ChangeLog** replaced by **NEWS.md**.
* Method `match_names` for `vegtable` objects.
* New function `layers2samples`.

### Improvements
* No column `LayerID` in prototype for `vegtable` objects.
* `NA` values are allowed for layers in slot `samples`.
* Layers are not any more restricted to single plots.
* Orphaned entries in slot `layers` will be deleted by function `clean`.
* Argument `match_header` included in funtion `veg_relation`.

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
