# Add information from slot 'layers' into slot 'samples'

Slot layers may include additional information that should be moved to
samples in order to use it by
[`subset()`](http://kamapu.github.io/vegtable/reference/subset.md),
[`aggregate()`](https://rdrr.io/r/stats/aggregate.html) or
[`crosstable()`](http://kamapu.github.io/vegtable/reference/crosstable.md)
methods.

If names of variables are not provided, all variables from the
respective layer table will be inserted in slot `samples`.

## Usage

``` r
layers2samples(object, layer, variable, ...)

# S4 method for class 'vegtable,character,character'
layers2samples(object, layer, variable, ...)

# S4 method for class 'vegtable,character,missing'
layers2samples(object, layer, variable, ...)
```

## Arguments

- object:

  An object of class
  [vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md).

- layer:

  Character value indicating a target layer.

- variable:

  Character vector with the names of variables to be inserted in slot
  `samples`.

- ...:

  Further arguments to be passed among methods.

## Value

An object of class
[vegtable](http://kamapu.github.io/vegtable/reference/vegtable-class.md)
with variables added to samples.

## Author

Miguel Alvarez <kamapu78@gmail.com>.
