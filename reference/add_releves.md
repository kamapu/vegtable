# Merge relevés from data frames into vegtable objects

Addition of plot observations into existing data sets may implicate
merging data frames with
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
objects.

Since this function will only update slots **samples** and **header**,
consistency with slots **layers**, **relations** and **species** have to
be checked and accordingly updated in advance.

## Usage

``` r
add_releves(vegtable, releves, ...)

# S4 method for class 'vegtable,data.frame'
add_releves(
  vegtable,
  releves,
  header,
  abundance,
  split_string,
  usage_ids = FALSE,
  layers = FALSE,
  layers_var,
  format = "crosstable",
  preserve_ids = FALSE,
  ...
)

add_releves(vegtable, ...) <- value

# S4 method for class 'vegtable,data.frame'
add_releves(vegtable, ...) <- value
```

## Arguments

- vegtable:

  An object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- releves:

  A data frame including plot observations to be added into `vegtable`.

- ...:

  Further arguments passed to function
  [`cross2db()`](http://kamapu.github.io/vegtable/reference/crosstable.md)
  (i.e. `na_strings`).

- header:

  A data frame (optional) including header information for plots.

- abundance:

  A character value (or vector of length 2) indicating the names of
  abundance variable in `vegtable`.

- split_string:

  Character value used to split mixed abundance codes.

- usage_ids:

  Logical value indicating whether species are as taxon usage ids
  (integers) or names in `releves`.

- layers:

  Logical value indicating whether layers are included in `releves` or
  not.

- layers_var:

  Name of the layer variable in `vegtable`.

- format:

  Character value indicating input format of `releves` (either
  `"crosstable"` or `"databaselist"`).

- preserve_ids:

  A logical value, whether IDs in input data set should used as
  `ReleveID` or not. Those IDs have to be integers and if one of those
  already exists in `vegtable`, an error will be retrieved.

- value:

  A data frame containing new plot observations. I is passed to
  parameter 'releves' by the replace method.

## See also

[`cross2db()`](http://kamapu.github.io/vegtable/reference/crosstable.md)

## Author

Miguel Alvarez <kamapu78@gmail.com>
